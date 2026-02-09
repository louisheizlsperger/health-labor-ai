#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Variable labels
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

variable_labels <- c(
  
  # Main MH institutionalization
  mh_inst                                 = "Institutionalized in 1880",
  mh_inst_1870                            = "Institutionalized in 1870",
  mh_inst_1880                            = "Institutionalized in MH in 1880",
  
  # Alternative institutionalization types
  prison_1870                             = "Institutionalized in prison in 1870",
  prison_1880                             = "Institutionalized in prison in 1880",
  almshouse_1870                          = "Institutionalized in almshouse in 1870",
  almshouse_1880                          = "Institutionalized in almshouse in 1880",
  any_inst_1880                           = "Institutionalized in any institution in 1880",
  
  # Outcomes (1880)
  famsize_tc                              = "Family size (top-coded at 15)",
  married_spouse_present                  = "Married, spouse present",
  labforce_in                             = "In the labor force",
  
  # Complier characteristics (1870)
  age_1870                                = "Age in 1870",
  age_1870_sq                             = "Age squared in 1870",
  race_white_1870                         = "White (1870)",
  race_black_1870                         = "Black or African American (1870)",
  race_other_1870                         = "Other race (1870)",
  sex_female_1870                         = "Female (1870)",
  famsize_tc_1870                         = "Family size in 1870 (top-coded at 15)",
  married_spouse_present_1870             = "Married, spouse present (1870)",
  labforce_in_1870                        = "In the labor force (1870)",
  
  # Instruments (continuous)
  dist_new_hospital_1871_1879_100km       = "Distance to nearest new MH hospital, 1871–79 (per 100 km)",
  dist_new_hospital_1871_1879_100km_capped= "Distance to new MH hospital, 1871–79 (per 100 km, capped at 200)",
  log_dist_new_hospital_1871_1879         = "Log distance to nearest new MH hospital, 1871–79",
  
  # Instruments (distance dummies)
  new_mh_25km                             = "New MH opened within 25 km (1871–79)",
  new_mh_50km                             = "New MH opened within 50 km (1871–79)",
  new_mh_100km                            = "New MH opened within 100 km (1871–79)",
  
  # County-level controls
  county_geo_pop_grow_1870_1880           = "County pop. growth, 1870–80 (annualized)",
  county_log_pop_1870                     = "Log county population, 1870",
  
  # Miscellaneous
  county_id                               = "County"
  
)
