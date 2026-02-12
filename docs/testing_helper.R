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

STTSTJ_sample <- getSampleData(2025:2025, "STTSTJ")

STTSTJ_stratum <- getStratumData(2025:2025, "STTSTJ")

taxonomic_data <- getTaxonomicData()

STTSTJ <- list(
  taxonomic_data = taxonomic_data,
  sample_data    = STTSTJ_sample,
  stratum_data   = STTSTJ_stratum)

#Map

STTSTJmap <- STTSTJ$sample_data %>%
  group_by(YEAR, PRIMARY_SAMPLE_UNIT, ADMIN, PROT) %>%
  summarise(lat = mean(LAT_DEGREES), lon = mean(LON_DEGREES))

write.csv(STTSTJmap, file = "STTSTJmap.csv", row.names = FALSE)


