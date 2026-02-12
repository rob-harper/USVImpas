# Strata table
render_strata_table <- function(df, caption = "Number of sites sampled by year") {

  table <- df %>%
    group_by(YEAR, PROT, STRAT) %>%
    summarise(n = n_distinct(PRIMARY_SAMPLE_UNIT), .groups = "drop") %>%
    mutate(description = case_when(
      STRAT == "AGRFDEEP" ~ "Aggregated reef, deep",
      STRAT == "AGRFSHLW" ~ "Aggregated reef, shallow",

      STRAT == "BDRKDEEP" ~ "Bedrock reef, deep",
      STRAT == "BDRKSHLW" ~ "Bedrock reef, shallow",

      STRAT == "HARDDEEP" ~ "Hardbottom, deep",
      STRAT == "HARDSHLW" ~ "Hardbottom, shallow",

      STRAT == "PTRFDEEP" ~ "Patch reef, deep",
      STRAT == "PTRFSHLW" ~ "Patch reef, shallow",

      STRAT == "PVMTDEEP" ~ "Pavement, deep",
      STRAT == "PVMTSHLW" ~ "Pavement, shallow",

      STRAT == "SCRDEEP"  ~ "Scattered coral and rock, deep",
      STRAT == "SCRSHLW"  ~ "Scattered coral and rock, shallow",
      TRUE ~ STRAT
    ),
    PROT = case_when(
      PROT == 0 ~ "Outside",
      PROT == 1 ~ "Inside",
      TRUE ~ as.character(PROT)
    ),
    PROT = factor(PROT, levels = c("Outside", "Inside"))) %>%
    select(YEAR, PROT, STRAT, description, n) %>%
    pivot_wider(names_from = YEAR, values_from = n, values_fill = 0) %>%
    arrange(PROT, STRAT) %>%
    ungroup()

  if (knitr::is_html_output()) {
    DT::datatable(
      table,
      class = "cell-border stripe",
      rownames = FALSE,
      colnames = c("Study Area", "Strata Name", "Strata Description", sort(unique(df$YEAR))),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold;',
        caption
      ),
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        info = FALSE,
        paging = FALSE,
        searching = FALSE
      )
    )
  } else {
    knitr::kable(
      table,
      caption = caption,
      booktabs = TRUE,
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "hold_position"),
        font_size = 10
      )
  }
}

# Species Table

render_species_table <- function(spp_list, caption = "Table 2: Fish Species") {

  # Add image paths if missing
  if (!"img_path" %in% colnames(spp_list)) {
    spp_list <- spp_list %>%
      mutate(img_path = file.path("species_photos", paste0(gsub(" ", "_", SPECIES_CD), ".png")))
  }

  if (knitr::is_html_output()) {
    # HTML table with clickable thumbnails
    spp_table_html <- spp_list %>%
      mutate(Photo = ifelse(
        file.exists(img_path),
        paste0(
          '<img src="', img_path,
          '" height="60" style="cursor:pointer;" onclick="showLightbox(\'', img_path, '\')">'
        ),
        ""
      )) %>%
      select(SPECIES_CD, COMNAME, SCINAME, Photo)

    htmltools::tagList(
      # Lightbox div + JS
      htmltools::HTML('
        <div id="lightbox" style="display:none;position:fixed;z-index:9999;left:0;top:0;width:100%;height:100%;background:rgba(0,0,0,0.8);justify-content:center;align-items:center;" onclick="this.style.display=\'none\'">
          <img id="lightbox-img" style="max-width:90%;max-height:90%;">
        </div>
        <script>
          function showLightbox(src){
            document.getElementById("lightbox-img").src = src;
            document.getElementById("lightbox").style.display = "flex";
          }
        </script>
      '),
      DT::datatable(
        spp_table_html,
        escape = FALSE,
        class = "cell-border stripe",
        rownames = FALSE,
        colnames = c("Species Code", "Common Name", "Scientific Name", "Photo"),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; font-weight: bold;',
          caption
        ),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          info = FALSE,
          paging = FALSE,
          searching = FALSE
        )
      ) %>%
        DT::formatStyle(columns = "SCINAME", fontStyle = "italic"))

  } else {
    # PDF table with images (blank if missing)
    spp_table_pdf <- spp_list %>%
      mutate(Photo = paste0("\\includegraphics[width=3cm]{", img_path, "}")) %>%
      select(SPECIES_CD, COMNAME, SCINAME, Photo)

    knitr::kable(
      spp_table_pdf,
      format = "latex",
      caption = caption,
      col.names = c("Species Code", "Common Name", "Scientific Name", "Photo"),
      booktabs = TRUE,
      escape = FALSE,
      longtable = TRUE
    ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "repeat_header"),
        font_size = 9
      ) %>%
      kableExtra::column_spec(3, italic = TRUE)
  }
}
#Density function
USVI_domain_dens_by_year <- function(dataset,
                                     species = NULL,
                                     length = NULL,
                                     year = NULL,
                                     title = NULL,
                                     caption = NULL) {

  if (is.null(species)) {
    stop("species must be provided and contain SPECIES_CD")
  }

  # number of strata per year and protection level
  strat_number <- dataset$stratum_data %>%
    reframe(strat_num = n_distinct(STRAT), .by = c(YEAR, PROT)) %>%
    mutate(
      YEAR = as_factor(YEAR),
      PROT = as.integer(PROT)
    )

  # get densities for each PROT level (defensively)
  dens <- purrr::map_dfr(c(0, 1, 2), function(p) {

    res <- tryCatch(
      getDomainDensity(
        dataset,
        species$SPECIES_CD,
        group = species,
        years = year,
        status = p,
        length_bins = length
      ),
      error = function(e) NULL
    )

    if (is.null(res) || nrow(res) == 0) return(NULL)

    res %>%
      mutate(
        PROT = p,
        SE   = sqrt(var),
        YEAR = as_factor(YEAR)
      )
  }) %>%
    filter(
      if (!is.null(length) && "length_class" %in% names(.))
        length_class == paste0(">= ", length)
      else TRUE
    ) %>%
    left_join(strat_number, by = c("YEAR", "PROT"))

  # guard against missing GROUP (rare but possible)
  if (!"GROUP" %in% names(dens)) {
    dens$GROUP <- "All"
  }

  # plot
  ggplot(
    dens,
    aes(
      x = YEAR,
      y = density,
      color = factor(PROT),
      group = factor(PROT)
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = density - SE, ymax = density + SE),
      width = 0.25,
      linewidth = 0.5
    ) +
    labs(
      title   = title,
      caption = caption,
      color   = "Protection level",
      x       = "Year",
      y       = "Relative Density (177 ind/m²)"
    ) +
    theme_Publication(base_size = 15) +
    theme(
      plot.caption = element_text(hjust = 0.5, size = 14),
      legend.text  = element_text(size = 12)
    ) +
    facet_wrap(~ GROUP, scales = "free_y")
}

#Occurrence function
USVI_domain_occ_by_year <- function(dataset,
                                    species = NULL,
                                    length = NULL,
                                    year = NULL,
                                    title = NULL,
                                    caption = NULL) {

  # number of strata per year and protection level
  strat_number <- dataset$stratum_data %>%
    reframe(strat_num = n_distinct(STRAT), .by = c(YEAR, PROT)) %>%
    mutate(YEAR = as_factor(YEAR))

  # get occurrence for each PROT level
  occ <- purrr::map_dfr(c(0, 1, 2), function(p) {
    getDomainOccurrence(dataset,
                        species$SPECIES_CD,
                        group = species,
                        years = year,
                        status = p,
                        length_bins = length) %>%
      mutate(
        PROT = p,
        SE   = sqrt(var),
        YEAR = as_factor(YEAR)
      )
  }) %>%
    filter(if (!is.null(length)) length_class == paste0(">= ", length) else TRUE) %>%
    left_join(strat_number, by = c("YEAR", "PROT"))

  # plot
  p <- ggplot(occ,
              aes(x = YEAR,
                  y = occurrence,
                  color = factor(PROT),
                  group = factor(PROT))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = occurrence - SE, ymax = occurrence + SE),
      width = 0.25,
      size = 0.5
    ) +
    labs(
      title   = title,
      caption = caption,
      color   = "Protection level",
      x       = "Year",
      y       = "Relative Occurrence"
    ) +
    theme_Publication(base_size = 15) +
    theme(
      plot.caption = element_text(hjust = 0.5, size = 14),
      legend.text  = element_text(size = 12)
    ) +
    facet_wrap(~ GROUP, scales = "free_y")

  return(p)
}

# Copy-pasteable: compute_bin_size, LF helpers, plotting, and render function.
# Requires: tidyverse (dplyr/tidyr/purrr), ggplot2, patchwork, plus your getDomainLengthFrequency() and labeler()/theme_Publication().

compute_bin_size <- function(max_size, target_bins = 10) {
  # Handle invalid input
  if (is.null(max_size) || is.na(max_size) || max_size <= 0) {
    return(5)
  }
  possible_bin_sizes <- c(2, 5, 10)
  num_bins <- max_size / possible_bin_sizes
  closest_match_index <- which.min(abs(num_bins - target_bins))
  possible_bin_sizes[closest_match_index]
}

# Robust bar-plot function for length-freq (category mode).
# Uses ggplot's default fill colors and default legend labels.
plot_bins <- function(x, ttle, bin_size, vline_at_lc = NULL, legend_mode = "category") {

  # safe maximum bin (handles NA / empty cases)
  max_bin <- suppressWarnings(max(x$bin, na.rm = TRUE))
  if (!is.finite(max_bin) || max_bin <= 0) {
    max_bin <- 0L
  } else {
    max_bin <- as.integer(max_bin)
  }

  # breaks/labels only when we have bins
  if (max_bin > 0) {
    br <- seq_len(max_bin)
    la <- labeler(bin_num = max_bin, bin_size = bin_size)
    x_scale <- scale_x_discrete(
      breaks = br,
      labels = la[seq_len(max_bin)],
      limits = factor(br)
    )
  } else {
    x_scale <- NULL
  }

  p <- ggplot(x, aes(x = factor(bin), y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge2", width = .9, color = "black", size = .5) +
    theme_Publication(base_size = 20) +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text = element_text(size = 12)
    ) +
    labs(title = ttle, fill = "Sampling Year") +
    ylab("Relative Frequency") +
    xlab("Fork Length (cm)")

  if (!is.null(x_scale)) p <- p + x_scale

  # add vline only if we can compute a sensible x-intercept
  if (!is.null(vline_at_lc) && !is.null(bin_size) && bin_size > 0 && max_bin > 0) {
    xint <- vline_at_lc / bin_size
    if (is.finite(xint) && xint >= 1 && xint <= max_bin) {
      p <- p + geom_vline(xintercept = xint, linetype = "longdash", size = 1)
    }
  }

  p
}

# Year-comparison bar-plot (fills by fill_key = "<code>_<YEAR>").
# Uses ggplot defaults for color selection and legend labels.
plot_bins_yr <- function(x, ttle, bin_size, vline_at_lc = NULL, category = NULL, spp_name = NULL, legend_mode = "year") {

  # safe maximum bin (handles NA / empty cases)
  max_bin <- suppressWarnings(max(x$bin, na.rm = TRUE))
  if (!is.finite(max_bin) || max_bin <= 0) {
    max_bin <- 0L
  } else {
    max_bin <- as.integer(max_bin)
  }

  if (max_bin > 0) {
    br <- seq_len(max_bin)
    la <- labeler(bin_num = max_bin, bin_size = bin_size)
    x_scale <- scale_x_discrete(
      breaks = br,
      labels = la[seq_len(max_bin)],
      limits = factor(br)
    )
  } else {
    x_scale <- NULL
  }

  p <- ggplot(x, aes(x = factor(bin), y = value, fill = fill_key)) +
    geom_bar(stat = "identity", position = "dodge2", width = 0.9, color = "black", size = 0.5) +
    theme_Publication(base_size = 20) +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text = element_text(size = 12)
    ) +
    labs(title = ttle, fill = "Sampling Year") +
    ylab("Relative Frequency") +
    xlab("Fork Length (cm)")

  if (!is.null(x_scale)) p <- p + x_scale

  # add vline only if we can compute a sensible x-intercept
  if (!is.null(vline_at_lc) && !is.null(bin_size) && bin_size > 0 && max_bin > 0) {
    xint <- vline_at_lc / bin_size
    if (is.finite(xint) && xint >= 1 && xint <= max_bin) {
      p <- p + geom_vline(xintercept = xint, linetype = "longdash", size = 1)
    }
  }

  p
}

# Length frequency for comparing inside/outside/other. Converts protected_status -> "0","1","2" (factor).
MIR_LF <- function(df, spp, bin_size, yrs = NULL, spp_name) {

  x <- getDomainLengthFrequency(df, species = spp, merge_protected = FALSE) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      # Ensure at least one length_class for empty data
                      full_join(., data.frame(length_class = if (nrow(.) > 0) seq(1, max(.$length_class), 0.5) else 1)) %>%
                      select(length_class, frequency) %>%
                      replace(is.na(.), 0) %>%
                      mutate(bin = as.numeric(cut(length_class,
                                                  breaks = seq(0, max(length_class, na.rm = TRUE) + bin_size, bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency, na.rm = TRUE))
    )) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
    ungroup() %>%
    mutate(
      value = freq,
      # Map protected_status to numeric-code strings: "0","1","2"
      variable = case_when(
        protected_status == 0 ~ "0",
        protected_status == 1 ~ "1",
        protected_status == 2 ~ "2",
        TRUE ~ as.character(protected_status)
      ) %>% factor(levels = c("0", "1", "2"))
    )

  # pivot to wide then back to long, but ensure at least one bin column exists
  y_wide <- x %>%
    filter(YEAR == yrs) %>%
    select(YEAR, SPECIES_CD, variable, bin, value) %>%
    pivot_wider(names_from = bin, values_from = value, values_fill = 0)

  # If pivot_wider produced only id cols (no bins), add a dummy bin "0"
  id_cols <- c("YEAR", "SPECIES_CD", "variable")
  other_cols <- setdiff(names(y_wide), id_cols)
  if (length(other_cols) == 0) {
    y_wide[["0"]] <- 0
  }

  y <- y_wide %>%
    pivot_longer(cols = -all_of(id_cols), names_to = "bin", values_to = "value") %>%
    mutate(bin = as.numeric(bin))

  # Final guard: if result is empty or bin all NA, create minimal row
  if (nrow(y) == 0 || all(is.na(y$bin))) {
    y <- tibble(YEAR = yrs, SPECIES_CD = spp, variable = factor("0", levels = c("0", "1", "2")), bin = 0, value = 0)
  }

  plot_bins(x = y, ttle = paste0(spp_name, " ", yrs), bin_size = bin_size, legend_mode = "category")
}

# Length frequency for comparing a single category across recent years
MIR_LF_yr <- function(df, spp, bin_size, yrs = NULL, spp_name, category, custom_title = NULL) {

  x <- getDomainLengthFrequency(df, species = spp, merge_protected = FALSE) %>%
    group_by(YEAR, SPECIES_CD, protected_status) %>%
    filter(YEAR %in% sort(unique(YEAR), decreasing = TRUE)[1:3]) %>%
    nest() %>%
    mutate(Lf = map(data, ~ .x %>%
                      data.frame() %>%
                      full_join(., data.frame(length_class = if (nrow(.) > 0) seq(1, max(.$length_class), 0.5) else 1)) %>%
                      select(length_class, frequency) %>%
                      replace(is.na(.), 0) %>%
                      mutate(bin = as.numeric(cut(length_class,
                                                  breaks = seq(0, max(length_class, na.rm = TRUE) + bin_size, bin_size)))) %>%
                      arrange(length_class) %>%
                      group_by(bin) %>%
                      summarise(freq = sum(frequency, na.rm = TRUE))
    )) %>%
    unnest(Lf) %>%
    select(YEAR, SPECIES_CD, protected_status, bin, freq) %>%
    ungroup() %>%
    mutate(
      value = freq,
      variable = case_when(
        protected_status == 0 ~ "0",
        protected_status == 1 ~ "1",
        protected_status == 2 ~ "2",
        TRUE ~ as.character(protected_status)
      ) %>% factor(levels = c("0", "1", "2"))
    )

  y_wide <- x %>%
    filter(YEAR %in% yrs, variable == category) %>%
    select(YEAR, SPECIES_CD, variable, bin, value) %>%
    pivot_wider(names_from = bin, values_from = value, values_fill = 0)

  # If no bin columns, add a dummy "0"
  id_cols <- c("YEAR", "SPECIES_CD", "variable")
  other_cols <- setdiff(names(y_wide), id_cols)
  if (length(other_cols) == 0) {
    first_year <- if (length(yrs) > 0) yrs[1] else NA
    y_wide <- tibble(YEAR = first_year, SPECIES_CD = spp, variable = category, `0` = 0)
  }

  y <- y_wide %>%
    pivot_longer(cols = -all_of(id_cols), names_to = "bin", values_to = "value") %>%
    mutate(
      bin = as.numeric(bin),
      fill_key = paste(variable, YEAR, sep = "_")
    )

  # Final guard
  if (nrow(y) == 0 || all(is.na(y$bin))) {
    first_year <- if (length(yrs) > 0) yrs[1] else NA
    y <- tibble(YEAR = first_year, SPECIES_CD = spp, variable = category, bin = 0, value = 0, fill_key = paste(category, first_year, sep = "_"))
  }

  if (is.null(custom_title)) custom_title <- paste0(spp_name, " - ", category)

  plot_bins_yr(x = y, ttle = custom_title, bin_size = bin_size, category = category, spp_name = spp_name, legend_mode = "year")
}

# Render collection of LF plots (top: individual years; bottom: combined category plots)
render_LF_plots <- function(df, SPECIES_CD, COMNAME, max_size = NULL, yrs = c(2022, 2024),
                            target_bins = 8, categories = c("1", "0", "2")) {

  # ---- Compute bin size ----
  bin_size <- if (!is.null(manual_bin) && SPECIES_CD %in% names(manual_bin)) {
    manual_bin[[SPECIES_CD]]
  } else {
    compute_bin_size(max_size, target_bins)
  }

  # Helper to coerce non-plot items to a spacer
  ensure_plot <- function(p) {
    ok <- inherits(p, c("gg", "ggplot", "patchwork"))
    if (isTRUE(ok)) return(p)
    patchwork::plot_spacer()
  }

  # Individual year panels
  panels <- lapply(yrs, function(year) {
    MIR_LF(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = year, spp_name = COMNAME)
  })
  panels <- lapply(panels, ensure_plot)

  # Combined category plots: build for requested categories (default includes "1","0","2")
  combined_cat_plots <- lapply(categories, function(cat) {
    title <- paste(COMNAME, "-", cat)
    p <- MIR_LF_yr(df = df, spp = SPECIES_CD, bin_size = bin_size, yrs = yrs,
                   spp_name = COMNAME, category = cat, custom_title = title)
    ensure_plot(p)
  })

  # layout construction (2 per row)
  n_per_row <- 3
  if (length(panels) == 0) panels <- list()

  n_needed <- n_per_row - (length(panels) %% n_per_row)
  if (n_needed != n_per_row) {
    panels <- c(panels, replicate(n_needed, patchwork::plot_spacer(), simplify = FALSE))
  }

  row_panels <- split(panels, ceiling(seq_along(panels) / n_per_row))
  combined_rows <- lapply(row_panels, function(row) {
    row <- lapply(row, ensure_plot)
    if (length(row) == 1) return(row[[1]])
    Reduce(`|`, row)
  })
  combined_years <- if (length(combined_rows) > 0) {
    if (length(combined_rows) == 1) combined_rows[[1]] else Reduce(`/`, combined_rows)
  } else {
    NULL
  }

  # bottom rows for category plots
  bottom_rows <- list()
  if (length(combined_cat_plots) > 0) {
    n_needed_cat <- n_per_row - (length(combined_cat_plots) %% n_per_row)
    if (n_needed_cat != n_per_row) {
      combined_cat_plots <- c(combined_cat_plots, replicate(n_needed_cat, patchwork::plot_spacer(), simplify = FALSE))
    }
    cat_row_panels <- split(combined_cat_plots, ceiling(seq_along(combined_cat_plots) / n_per_row))
    bottom_rows <- lapply(cat_row_panels, function(row) {
      row <- lapply(row, ensure_plot)
      if (length(row) == 1) return(row[[1]])
      Reduce(`|`, row)
    })
  }

  bottom_row <- NULL
  if (length(bottom_rows) == 1) {
    bottom_row <- bottom_rows[[1]]
  } else if (length(bottom_rows) > 1) {
    bottom_row <- Reduce(`/`, bottom_rows)
  }

  # stack top and bottom
  final_plot <- NULL
  if (!is.null(combined_years) && !is.null(bottom_row)) {
    final_plot <- combined_years / bottom_row
  } else if (!is.null(combined_years)) {
    final_plot <- combined_years
  } else {
    final_plot <- bottom_row
  }

  invisible(final_plot)
}

# Manually adjust the bin size based on species code (unchanged)
manual_bin <- c("HAE FLAV" = 5, "CEP CRUE" = 5, "CAL CALA" = 5, "CAL NODO" = 5)
