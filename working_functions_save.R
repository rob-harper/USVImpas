

```{r setup_data_STX, include=FALSE}
# SETUP CHUNK: We run this first, silently, so the data is ready for the tabs below.
library(dplyr)
library(purrr)
library(stringr)

target_species <- c("OCY CHRY", "EPI GUTT", "EPI STRI", "BAL VETU")

spp_list_STX <- STX$taxonomic_data %>%
  filter(SPECIES_CD %in% target_species) %>%
  select(SPECIES_CD, COMNAME, SCINAME) %>%
  distinct() %>%
  arrange(match(SPECIES_CD, target_species)) %>%
  left_join(community_data) %>%
  mutate(
    COMNAME = stringr::str_to_title(COMNAME),
    img_path = file.path("species_photos", paste0(gsub(" ", "_", SPECIES_CD), ".png"))
  )


# St. Thomas and St. John

UM took away my arc license again… waiting for Jesus to give me laptop on Monday

## Strata

To allow for direct comparisons of fish communities inside and outside of marine protected areas on St. Thomas and St. John, bedrock deep and shallow strata were combined to create adequate sample sizes within each level of protection. Data collected by the diver pair were averaged at the site level. Standard fish metrics, including relative density, occurrence, and length composition, were evaluated for a suite of selected individual fish species. Computational formulas of standard metrics for a single-stage stratified random sampling design are modified from [@Smith2011] and provided in detail in [@Grove2021] and [@Bryan2016]. Fish analysis scripts are open source and available through the [NCRMP Fish R package](https://github.com/jeremiaheb/rvc) [@Ganz2015].


```{r echo=FALSE}
render_strata_table_STTSTJ(STTSTJ$sample_data,
                           caption = "Table 1: Number of reef fish survey sites in each stratum inside and outside St. Thomas and St. John Marine Protected Areas ")
```
## Species

```{r echo=FALSE}
# To add species to the document, add species code here
target_species <- c(
  "OCY CHRY", "EPI GUTT", "EPI STRI", "BAL VETU"
)

# Build spp_list dynamically with image paths for later use in tables/plots

spp_list_STTSTJ <- STTSTJ$taxonomic_data %>%
  filter(SPECIES_CD %in% target_species) %>%
  select(SPECIES_CD, COMNAME, SCINAME) %>%
  distinct() %>%
  arrange(match(SPECIES_CD, target_species)) %>%
  left_join(community_data) %>%
  mutate(
    COMNAME = stringr::str_to_title(COMNAME),
    img_path = file.path("species_photos", paste0(gsub(" ", "_", SPECIES_CD), ".png"))
  )

render_species_table(spp_list_STTSTJ, caption =
                       "Table 3:  Fish species with representative photos.")
```

## Density

Relative density is an index or relative measure of population density that is commonly used in fisheries surveys. Relative density is a comparative measure of density across locations or years, rather than an absolute count that is impractical due to the scale and complexity of the marine environment. Densities shown here represent NCRMP’s fish survey area (177m2).


Density specific text to this proj

```{r, fig.width=12, fig.height=8, echo=FALSE}
USVI_domain_dens_by_year(STTSTJ, spp_list_STTSTJ, caption = str_wrap(
  "Update caption text", width = 100), legend_labels = c("Open", "NPS", "STEER"))
```

## Occurrence

Monitoring species occurrence, or how often a species is detected in surveys, offers valuable insight into its distribution. This metric provides presence data regardless of abundance, which helps to identify whether a species is widespread or rare. For resource managers, species occurrence can be used as a bioindicator of overall ecosystem health and to evaluate the success of restoration projects. A consistently higher occurrence of a fish species, especially of a rare species or an obligate coral dweller, within coral restoration areas serves as an indicator of high-quality habitat.

```{r, fig.width=12, fig.height=8, echo=FALSE}
USVI_domain_occ_by_year(STTSTJ, spp_list_STTSTJ, caption = str_wrap(
  "Update caption text", width = 100), legend_labels = c("Open", "NPS", "STEER"))
```

## Length Frequency
Length compositions provide a detailed description of a fish’s population structure. These highly informative figures can show the length at which a species recruits to the coral reef from their nursery habitat, the length classes that are selected by local recreational and commercial fisheries, and the effectiveness of fisheries management regulations. Successful marine spatial protection can yield greater densities and larger sizes of fishery target species.

Relative length frequency by fork length (cm) bin is shown for each species within each sampling domain (i.e., STEER, NPS, and open) and between each sampling domain by survey year.

::: {.panel-tabset}

```{r}
#| results: asis
#| echo: false
#| warning: false
#| message: false
#| fig-width: 10
#| out-width: "100%"

pwalk(
  spp_list_STTSTJ,
  function(SPECIES_CD, COMNAME, max_size, ...) {

    # 1. Create the tab header
    # {.unlisted} hides it from the Table of Contents
    # {.unnumbered} prevents "1.1, 1.2" numbering
    cat("\n###", COMNAME, "{.unlisted .unnumbered}\n\n")

    # 2. Generate the plot with custom labels
    p <- render_LF_plots_simple(
      df            = STTSTJ,
      SPECIES_CD    = SPECIES_CD,
      COMNAME       = COMNAME,
      max_size      = max_size,
      yrs           = unique(STTSTJ$sample_data$YEAR),
      target_bins   = 12,
      legend_labels = c("Open", "NPS", "STEER") # Your custom labels go here!
    )

    # 3. Print the plot object explicitly
    if (!is.null(p)) {
      print(p)
    }

    # 4. CRITICAL: Add extra newlines to separate this tab from the next
    cat("\n\n")
  }
)
```

:::

  # St. Croix

  ## EEMP

  UM took away my arc license again… waiting for Jesus to give me laptop on Monday



\newpage


## Strata

To allow for direct comparisons of fish communities inside and outside of marine protected areas on St. Croix, the broader NCRMP dataset, was restricted to strata types that are found within EEMP. The dataset used in this analysis contains shallow aggregated, patch, and pavement reef. To achieve an adequate number of samples in each level of protection, certain strata were combined (i.e. scattered coral and rock + pavement, bedrock + aggregated reef).

Data collected by the diver pair were averaged at the site level. Standard fish metrics, including relative density, occurrence, and length composition, were evaluated for a suite of selected individual fish species. Computational formulas of standard metrics for a single-stage stratified random sampling design are modified from [@Smith2011] and provided in detail in [@Grove2021] and [@Bryan2016]. Fish analysis scripts are open source and available through the [NCRMP Fish R package](https://github.com/jeremiaheb/rvc) [@Ganz2015].

```{r echo=FALSE}
render_strata_table_STTSTJ(STX$sample_data,
                           caption = "Table 2: Number of reef fish survey sites in each stratum inside and outside St. Croix Marine Protected Areas")
```
## Species

```{r echo=FALSE}
# To add species to the document, add species code here
target_species <- c(
  "OCY CHRY", "EPI GUTT", "EPI STRI", "BAL VETU"
)

# Build spp_list dynamically with image paths for later use in tables/plots

spp_list_STX <- STX$taxonomic_data %>%
  filter(SPECIES_CD %in% target_species) %>%
  select(SPECIES_CD, COMNAME, SCINAME) %>%
  distinct() %>%
  arrange(match(SPECIES_CD, target_species)) %>%
  left_join(community_data) %>%
  mutate(
    COMNAME = stringr::str_to_title(COMNAME),
    img_path = file.path("species_photos", paste0(gsub(" ", "_", SPECIES_CD), ".png"))
  )

render_species_table(spp_list_STX, caption =
                       "Table 3:  Fish species with representative photos.")
```

## Density

Relative density is an index or relative measure of population density that is commonly used in fisheries surveys. Relative density is a comparative measure of density across locations or years, rather than an absolute count that is impractical due to the scale and complexity of the marine environment. Densities shown here represent NCRMP’s fish survey area (177m2).



```{r, fig.width=12, fig.height=8, echo=FALSE}
USVI_domain_dens_by_year(STX, spp_list_STX, caption = str_wrap(
  "Update caption text", width = 100), legend_labels = c("Open", "NPS", "EEMP"))
```

## Occurrence

Monitoring species occurrence, or how often a species is detected in surveys, offers valuable insight into its distribution. This metric provides presence data regardless of abundance, which helps to identify whether a species is widespread or rare. For resource managers, species occurrence can be used as a bioindicator of overall ecosystem health and to evaluate the success of no-take marine protected areas. A consistently higher occurrence of a fish species, especially of a rare species, within a no-take MPA can serve as an indicator of management success.


```{r, fig.width=12, fig.height=8, echo=FALSE}
USVI_domain_occ_by_year(STX, spp_list_STX, caption = str_wrap(
  "Update caption text", width = 100), legend_labels = c("Open", "NPS", "EEMP"))
```

## Length Frequency

Length compositions provide a detailed description of a fish’s population structure. These highly informative figures can show the length at which a species recruits to the coral reef from their nursery habitat, the length classes that are selected by local recreational and commercial fisheries, and the effectiveness of fisheries management regulations. Successful marine spatial protection can yield greater densities and larger sizes of fishery target species.

Relative length frequency by fork length (cm) bin is shown for each species within each sampling domain (i.e., EEMP, NPS, and open) and between each sampling domain by survey year.

::: {.panel-tabset}

```{r}
#| results: asis
#| echo: false
#| warning: false
#| message: false
#| fig-width: 10
#| out-width: "100%"

pwalk(
  spp_list_STX,
  function(SPECIES_CD, COMNAME, max_size, ...) {

    # 1. Create the tab header
    # {.unlisted} hides it from the Table of Contents
    # {.unnumbered} prevents "1.1, 1.2" numbering
    cat("\n###", COMNAME, "{.unlisted .unnumbered}\n\n")

    # 2. Generate the plot with custom labels
    p <- render_LF_plots_simple(
      df            = STX,
      SPECIES_CD    = SPECIES_CD,
      COMNAME       = COMNAME,
      max_size      = max_size,
      yrs           = unique(STX$sample_data$YEAR),
      target_bins   = 12,
      legend_labels = c("Open", "NPS", "EEMP") # Your custom labels go here!
    )

    # 3. Print the plot object explicitly
    if (!is.null(p)) {
      print(p)
    }

    # 4. CRITICAL: Add extra newlines to separate this tab from the next
    cat("\n\n")
  }
)
```

:::


