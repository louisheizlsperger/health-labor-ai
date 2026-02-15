library(dplyr)

# Load QWI data
qwi_raw <- read.csv("data_raw/QWI-raw-data/qwi.csv")

# Keep county level and 2010+
qwi_raw <- qwi_raw %>%
  filter(
    geo_level == "N",
    year >= 2010
  )

# Keep industries of interest
# 0 = total, 621 = ambulatory health care, 622 = hospitals, 623 = nursing and residential care, 624 = social assistance
qwi_raw <- qwi_raw %>%
  filter(industry %in% c(0, 621, 622, 623, 624))

# Build panel
qwi_panel <- qwi_raw %>%
  select(geography, industry, year, quarter, Emp, Sep, HirA) %>%
  mutate(
    sep_rate  = ifelse(Emp > 0, Sep / Emp, NA),
    hire_rate = ifelse(Emp > 0, HirA / Emp, NA)
  )

# Save
write.csv(
  qwi_panel,
  "build/output_external_storage/qwi_county_quarter_naics.csv",
  row.names = FALSE
)
