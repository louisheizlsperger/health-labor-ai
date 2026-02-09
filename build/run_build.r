#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Central build run file
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#=#=#=#=#=#=#=#=#=#=#=#
#### CALL RSCRIPTS ####
#=#=#=#=#=#=#=#=#=#=#=#

main <- function() {
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Provide summary stats of all raw census files
  #=#=#=#=#=#=#=#=#=#=#=#  
  
  # source(file.path(build, "code", "summary-stats-census-files.r"))
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Build county x year aggregates from full-count raw IPUMS census files
  #=#=#=#=#=#=#=#=#=#=#=#  
  
  # Produce county-year level counts: population, number of MH patients, ...
  # source(file.path(build, "code", "aggregate-county-year-pop-counts-from-microdata.r"))
  
  # Means of traits and demographics by county and year for IV complier analysis
  source(file.path(build, "code", "aggregate-county-year-covariates-stats-from-microdata.r"))
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Link adjacent census years
  #=#=#=#=#=#=#=#=#=#=#=#
  
  # source(file.path(build, "code", "link-adjacent-census-years-clp-and-althoff.r"))
  
  # source(file.path(build, "code", "augment-additional-census-variables.r"))
  

  #=#=#=#=#=#=#=#=#=#=#=#

  notify("mh-inst: Build finished running.")
  
}

#=#=#=#=#=#=#=#=#=#=#
#### EXECUTE ####
#=#=#=#=#=#=#=#=#=#=#

main() 

