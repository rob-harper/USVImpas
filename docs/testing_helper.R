USVImpas_data_dummy <- USVImpas_data

# Randomly assign 0, 1, or 2 to prot
set.seed(123)  # optional, for reproducibility
USVImpas_data_dummy$sample_data <- USVImpas_data_dummy$sample_data %>%
  dplyr::mutate(
    PROT = sample(c(0, 1, 2), size = n(), replace = TRUE),
    NUM = NUM + PROT*rnorm(n(), 10, 1)
  )


# Save dummy RDS
saveRDS(USVImpas_data_dummy, "data/USVImpas_data_dummy.rds")

#dry tort testing
DRY_tort_sample <- getSampleData(2016:2024, "DRY TORT")
DRY_tort_stratum <- getStratumData(2016:2024, "DRY TORT")


DryTort_data <- list(
  taxonomic_data = taxonomic_data,
  sample_data    = DRY_tort_sample,
  stratum_data   = DRY_tort_stratum
)

saveRDS(DryTort_data, "data/DryTort_data.rds")

library(rvc)
library(tidyverse)
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
