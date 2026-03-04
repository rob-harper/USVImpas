library(rvc)
library(tidyverse)
library(dplyr)


#dry tort testing
DRY_tort_sample <- getSampleData(2016:2024, "DRY TORT")
DRY_tort_stratum <- getStratumData(2016:2024, "DRY TORT")


DryTort_data <- list(
  taxonomic_data = taxonomic_data,
  sample_data    = DRY_tort_sample,
  stratum_data   = DRY_tort_stratum
)

saveRDS(DryTort_data, "data/DryTort_data.rds")


STTSTJ_sample <- getSampleData(2025:2025, "STTSTJ")

STTSTJ_stratum <- getStratumData(2025:2025, "STTSTJ")

taxonomic_data <- getTaxonomicData()

STTSTJ <- list(
  taxonomic_data = taxonomic_data,
  sample_data    = STTSTJ_sample,
  stratum_data   = STTSTJ_stratum)

STTSTJ$sample_data <- STTSTJ$sample_data %>%
  mutate(
    PROT = case_when(
      ADMIN == "OPEN"  ~ 0,
      ADMIN == "STEER" ~ 1,
      ADMIN %in% c("VICR", "VIIS") ~ 2,
      TRUE ~ NA_real_
    )
  )

saveRDS(STTSTJ, "data/STTSTJ.rds")

STX_sample <- getSampleData(2025:2025, "STX")

STX_stratum <- getStratumData(2025:2025, "STX")

STX <- list(
  taxonomic_data = taxonomic_data,
  sample_data    = STX_sample,
  stratum_data   = STX_stratum)

#map
STXmap <- STX$sample_data %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT, ADMIN, PROT, MAPGRID_NR, STRAT, DEPTH_STRAT) %>%
  summarise(lat = mean(LAT_DEGREES), lon = mean(LON_DEGREES))

write.csv(STXmap, file = "STXmap.csv", row.names = FALSE)

#Map

STTSTJmapsample <- STTSTJ$sample_data %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT, ADMIN, PROT, MAPGRID_NR, STRAT, DEPTH_STRAT) %>%
  summarise(lat = mean(LAT_DEGREES), lon = mean(LON_DEGREES))

STTSTJmapstrat <- STTSTJ$stratum_data

write.csv(STTSTJmapsample, file = "STTSTJmapsample.csv", row.names = FALSE)

write.csv(STTSTJmapstrat, file = "STTSTJmapstrat.csv", row.names = FALSE)


plot_domain_den_by_year_by_prot(STTSTJ, "EPI STRI")

library(dplyr)
library(stringr)

library(dplyr)
library(stringr)



STT_STJ_strat_table2025 <- STTSTJ$sample_data %>%
  distinct(PRIMARY_SAMPLE_UNIT, STRAT, ADMIN) %>%
  mutate(
    ADMIN = case_when(
      ADMIN %in% c("VICR", "VIIS") ~ "NPS",
      TRUE ~ ADMIN
    )
  ) %>%
  count(STRAT, ADMIN)

STTSTJnewstrat_combined <- STTSTJnewstrat %>%
  mutate(
    STRATxPROT = case_when(
      str_ends(STRATxPROT, "VICR") ~ str_replace(STRATxPROT, "VICR$", "PARK"),
      str_ends(STRATxPROT, "VIIS") ~ str_replace(STRATxPROT, "VIIS$", "PARK"),
      TRUE ~ STRATxPROT
    )
  )

STTSTJnewstrat_combined <- STTSTJnewstrat_combined %>%
  group_by(STRATxPROT) %>%
  summarise(FREQUENCY = sum(FREQUENCY), .groups = "drop")

STTSTJnewstrat_combined <- STTSTJnewstrat %>%
  mutate(
    STRATxPROT = case_when(
      str_ends(STRATxPROT, "VICR") ~ str_replace(STRATxPROT, "VICR$", "PARK"),
      str_ends(STRATxPROT, "VIIS") ~ str_replace(STRATxPROT, "VIIS$", "PARK"),
      TRUE ~ STRATxPROT
    )
  )

STTSTJnewstrat_combined <- STTSTJnewstrat_combined %>%
  group_by(STRATxPROT) %>%
  summarise(FREQUENCY = sum(FREQUENCY), .groups = "drop")

STTSTJnewstrat_combined<- STTSTJnewstrat_combined %>%
  mutate(
    # Extract protection first (if not already done)
    prot = case_when(
      str_ends(STRATxPROT, "OPEN")  ~ 0,
      str_ends(STRATxPROT, "PARK")  ~ 1,
      str_ends(STRATxPROT, "STEER") ~ 2
    ),

    # Remove protection suffix
    temp_strat = str_remove(STRATxPROT, "OPEN$|PARK$|STEER$"),

    # Extract depth
    DEPTH = str_extract(temp_strat, "^DEEP|^SHLW"),

    # Extract habitat (everything after depth)
    HABITAT = str_remove(temp_strat, "^DEEP|^SHLW"),

    # Reorder to HABITAT + DEPTH
    STRAT = paste0(HABITAT, DEPTH)
  ) %>%
  select(-temp_strat, -DEPTH, -HABITAT)

STTSTJnewstrat_combined <- STTSTJnewstrat_combined %>%
  mutate(
    DEPTH   = str_extract(STRAT, "^DEEP|^SHLW"),
    HABITAT = str_remove(STRAT, "^DEEP|^SHLW"),
    STRAT   = paste0(HABITAT, DEPTH)
  ) %>%
  select(-DEPTH, -HABITAT)

STT_STJ_strat_table2025 <- STT_STJ_strat_table2025 %>%
  mutate(
    prot = case_when(
      ADMIN == "OPEN"  ~ 0,
      ADMIN == "NPS"   ~ 1,
      ADMIN == "STEER" ~ 2
    )
  )

# Combined strat table for comparing samples vs all strat

STTSTJ_combined_strat_sample <- full_join(
  STTSTJnewstrat_combined,
  STT_STJ_strat_table2025,
  by = c("STRAT", "prot")
) %>%
  mutate(
    NTOT = ifelse(is.na(NTOT), 0, NTOT),
    n    = ifelse(is.na(n), 0, n)
  ) %>%
  arrange(STRAT, prot)

## script for comining strats
STTSTJnewstrat_combined <- STTSTJnewstrat_combined %>%
  mutate(
    STRATxPROT = case_when(
      str_starts(STRATxPROT, "DEEPBDRK") ~ str_replace(STRATxPROT, "DEEPBDRK", "BDRK"),
      str_starts(STRATxPROT, "SHLWBDRK") ~ str_replace(STRATxPROT, "SHLWBDRK", "BDRK"),
      TRUE ~ STRATxPROT
    )
  ) %>%
  group_by(STRATxPROT) %>%
  summarise(FREQUENCY = sum(FREQUENCY), .groups = "drop")

STTSTJnewstrat_combined <- STTSTJnewstrat_combined %>%
  mutate(
    STRATxPROT = case_when(
      str_starts(STRATxPROT, "PVMTDEEP") ~ str_replace(STRATxPROT, "DEEPPVMT", "PVMT"),
      str_starts(STRATxPROT, "SHLWPVMT") ~ str_replace(STRATxPROT, "SHLWPVMT", "PVMT"),
      TRUE ~ STRATxPROT
    )
  ) %>%
  group_by(STRATxPROT) %>%
  summarise(FREQUENCY = sum(FREQUENCY), .groups = "drop")


## STX


STX_trimmed <- STX_points_notake %>%
  select(PRIMARY_SAMPLE_UNIT, YEAR, ADMIN, PROT, STRAT, NoTake)

STX_trimmed <- STX_trimmed %>%
  mutate(
    PROT = case_when(
      ADMIN == "OPEN" ~ 0,
      ADMIN %in% c("BUIS", "SARI") ~ 1,
      ADMIN == "EEMP" ~ 0,   # default EEMP to 0
      TRUE ~ NA_real_
    )
  )

STX_trimmed <- STX_trimmed %>%
  mutate(
    PROT = if_else(ADMIN == "EEMP" & NoTake == 1, 2, PROT)
  )

STX_trimmed <- STX_trimmed %>%
  mutate(
    PROT = if_else(PRIMARY_SAMPLE_UNIT %in% c(4613, 4611, 4633, 4704),
                   2,
                   PROT)
  )

STX_strat_summary <- STX_trimmed %>%
  count(PROT, STRAT, name = "n") %>%
  select(PROT, STRAT, n)

STX_prot_strat_counts <- STX_sampleframe_notake %>%

  # 1️⃣ Create STRAT
  mutate(
    STRAT = paste0(HABITAT, DEPTH)
  ) %>%

  # 2️⃣ Create PROT (two-step logic)
  mutate(
    PROT = case_when(
      ADMIN == "OPEN" ~ 0,
      ADMIN %in% c("BUIS", "SARI") ~ 1,
      ADMIN == "EEMP" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    PROT = if_else(ADMIN == "EEMP" & NoTake == 1, 2, PROT)
  ) %>%

  # 3️⃣ Count GRID_ID per PROT × STRAT
  count(PROT, STRAT, name = "n")


## data organization final step

STTSTJ <- getRvcData(2025, "STTSTJ")

STTSTJ$sample_data <- STTSTJ$sample_data %>%
  mutate(PROT = case_when(
    ADMIN == "OPEN" ~ 0,
    ADMIN %in% c("VICR", "VIIS") ~ 1,
    ADMIN == "STEER" ~ 2
  ))

STTSTJ$stratum_data <- STTSTJ$stratum_data %>%
  mutate(NTOT = case_when(
    PROT == 1 & STRAT == "SCRSHLW" ~ NTOT + NTOT[PROT == 1 & STRAT == "PVMTSHLW"],
    TRUE ~ NTOT # Keep all other values the same
  ))

STTSTJ$stratum_data <- STTSTJ$stratum_data %>%
  filter(!(PROT == 1 & STRAT == "PVMTSHLW"))

STTSTJ$stratum_data <- STTSTJ$stratum_data %>%
  mutate(
    STRAT = case_when(
      STRAT %in% c("BDRKDEEP", "BDRKSHLW") ~ "BDRK",
      TRUE ~ STRAT
    )
  )
STTSTJ$stratum_data <- STTSTJ$stratum_data %>%
  group_by(REGION, YEAR, PROT, STRAT, STAGE_LEVEL) %>%
  summarise(
    NTOT = sum(NTOT, na.rm = TRUE),
    GRID_SIZE = sum(GRID_SIZE, na.rm = TRUE),
    .groups = "drop"
  )

STTSTJ$sample_data <- STTSTJ$sample_data %>%
  mutate(
    STRAT = case_when(
      STRAT %in% c("BDRKDEEP", "BDRKSHLW") ~ "BDRK",
      TRUE ~ STRAT
    )
  )

##sample data check
STTSTJ$sample_data %>%
  distinct(PRIMARY_SAMPLE_UNIT, PROT, STRAT) %>%
  count(PROT, STRAT) %>%
  arrange

##cv table

spp_vec <- c("EPI STRI", "OCY CHRY", "BAL VETU", "SPA VIRI")  # your species list

density_table <- map_dfr(spp_vec, function(spp) {

  getDomainDensity(STTSTJ, spp, merge_protected = FALSE) %>%
    mutate(
      species = spp,
      cv = (sqrt(var) / density) * 100
    )

})

saveRDS(STTSTJ, file = "STTSTJ_03-04.rds")

#STX

STX <- getRvcData(2025, "STX")

STX_sampleframe_notake <- STX_sampleframe_notake %>%
  mutate(
    STRAT = paste0(HABITAT, DEPTH)
  )
STX_sampleframe_notake <- STX_sampleframe_notake %>%
  mutate(
    PROT = case_when(
      ADMIN == "OPEN" ~ 0,
      ADMIN %in% c("BUIS", "SARI") ~ 1,
      ADMIN == "EEMP" & NoTake == 1 ~ 2,
      ADMIN == "EEMP" & NoTake == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

STX_strat_3.4 <- STX_sampleframe_notake %>%
  group_by(REGION, PROT, STRAT) %>%
  summarise(
    NTOT = n(),  # number of grid cells
    GRID_SIZE = sum(Shape_Area, na.rm = TRUE),
    .groups = "drop"
  )

STX_strat_3.4 <- STX_strat_3.4 %>%
  # Step 1: fix misassigned rows
  mutate(
    STRAT = case_when(
      PROT == 2 & STRAT == "eSHLW" ~ "PVMTSHLW",
      TRUE ~ STRAT
    ),
    PROT = case_when(
      is.na(PROT) & STRAT == "PVMTSHLW" ~ 2,
      TRUE ~ PROT
    ),
    REGION = case_when(
      is.na(REGION) & STRAT == "PVMTSHLW" ~ "STX",
      TRUE ~ REGION
    )
  ) %>%
  # Step 2: collapse duplicates
  group_by(REGION, PROT, STRAT) %>%
  summarise(
    NTOT = sum(NTOT, na.rm = TRUE),
    GRID_SIZE = sum(GRID_SIZE, na.rm = TRUE),
    .groups = "drop"
  )

STX_strat_3.4 <- STX_strat_3.4 %>%
  filter(!grepl("DEEP", STRAT))

STX_strat_3.4 <- STX_strat_3.4 %>%
mutate(
  STRAT = if_else(STRAT == "BDRKSHLW", "AGRSHLW", STRAT)
) %>%
  group_by(REGION, PROT, STRAT) %>%
  summarise(
    NTOT = sum(NTOT, na.rm = TRUE),
    .groups = "drop"
  )

STX_strat_3.4 <- STX_strat_3.4 %>%
  mutate(
    STRAT = if_else(STRAT == "AGRSHLW", "AGRFSHLW", STRAT)
  ) %>%
  group_by(REGION, PROT, STRAT) %>%
  summarise(
    NTOT = sum(NTOT, na.rm = TRUE),
    .groups = "drop"
  )

STX_strat_3.4 <- STX_strat_3.4 %>%
  mutate(
    STRAT = if_else(STRAT == "SCRSHLW", "PVMTSHLW", STRAT)
  ) %>%
  group_by(REGION, PROT, STRAT) %>%
  summarise(
    NTOT = sum(NTOT, na.rm = TRUE),
    .groups = "drop"
  )

## STX sample
STX_points_notake <- STX_points_notake %>%
  mutate(
    PROT = case_when(
      ADMIN == "OPEN" ~ 0,
      ADMIN %in% c("BUIS", "SARI") ~ 1,
      ADMIN == "EEMP" & NoTake == 0 ~ 0,
      ADMIN == "EEMP" & NoTake == 1 ~ 2,
      TRUE ~ NA_real_  # catches anything unexpected
    )
  )

STX_points_notake <- STX_points_notake %>%
  mutate(
    PROT = if_else(PRIMARY_SAMPLE_UNIT %in% c(4613, 4611, 4633, 4707),
                   2,   # set PROT to 2 for these PSUs
                   PROT)  # keep existing PROT for all others
  )

STX$sample_data <- STX$sample_data %>%
  # Join PROT from STX_points_notake based on PRIMARY_SAMPLE_UNIT
  left_join(
    STX_points_notake %>% select(PRIMARY_SAMPLE_UNIT, PROT_new = PROT),
    by = "PRIMARY_SAMPLE_UNIT"
  ) %>%
  # Replace the old PROT with the new PROT where available
  mutate(
    PROT = coalesce(PROT_new, PROT)  # use PROT_new if it exists, otherwise keep old PROT
  ) %>%
  select(-PROT_new)  # remove the temporary column
