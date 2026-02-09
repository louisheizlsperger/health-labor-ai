#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Central run file
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#=#=#=#=#=#=#=#=#=#=#=#
#### CALL RSCRIPTS ####
#=#=#=#=#=#=#=#=#=#=#=#

main <- function() {
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Case Study
  #=#=#=#=#=#=#=#=#=#=#=#
  
  # Produce IPUMS Census counts for MH institutionalized by county and year
  source(file.path(analysis_code, "produce-ipums-census-counts-by-county-year.r"))
  
  # Plot time series of intsitutionalization rates near large state hospital openings
  source(file.path(analysis_code, "case-studies-time-series-plots.r"))

  #=#=#=#=#=#=#=#=#=#=#=#

  notify("mh-inst: Analysis finished running.")
  
}

#=#=#=#=#=#=#=#=#=#=#
#### EXECUTE ####
#=#=#=#=#=#=#=#=#=#=#

main() 

