#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Central run file
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Clean environment
rm(list = ls())

# Set seed
set.seed(1900)


script_dir <- function() {
  
  # 1) VS Code / RStudio (active editor)
  p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "") 
  if (nzchar(p)) return(dirname(p))

  # 2) Source()'d file
  p <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) "")
  if (nzchar(p)) return(dirname(p))

  # 3) Knitr/Rmarkdown
  p <- tryCatch(normalizePath(knitr::current_input()), error = function(e) "")
  if (nzchar(p)) return(dirname(p))

  # 4) Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg))))

}

main <- script_dir()
setwd(main)

config <- file.path(main, "config")

build <- file.path(main, "build")
build_output <- file.path(build, "output")
build_output_external <- file.path(build, "output_external_storage")
data_raw <- file.path(main, "data_raw")

# Toy data toggle ####

# Set to TRUE to use small toy datasets stored inside the repo (data_raw_toy, output_external_storage_toy).
use_toy_data <- FALSE

if (use_toy_data) {
  message("Running in TOY mode: using data_raw_toy and output_external_storage_toy.")
  data_raw      <- file.path(main, "data_raw_toy")
  build_output  <- file.path(build, "output_external_storage_toy")
}


analysis <- file.path(main, "analysis")
analysis_code <- file.path(analysis, "code")
analysis_output <- file.path(analysis, "output")

#=#=#=#=#=#=#=#=#=#=#=#
#### CALL RSCRIPTS ####
#=#=#=#=#=#=#=#=#=#=#=#

main <- function(build_data = FALSE,
                 run_analysis = FALSE) {
  
  # Configuration
  source(file.path(config, "config_r.r"))
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # BUILD
  #=#=#=#=#=#=#=#=#=#=#=#
  
  if (build_data) {
    
    source(file.path(build, "run_build.r"))
    
  }

  #=#=#=#=#=#=#=#=#=#=#=#
  # ANALYSIS
  #=#=#=#=#=#=#=#=#=#=#=#
  
  if (run_analysis) {
    
   source(file.path(analysis, "run_analysis.r"))
    
  }

  # notify("mh-inst: Code finished running.")
  
}

#=#=#=#=#=#=#=#=#=#=#
#### EXECUTE ####
#=#=#=#=#=#=#=#=#=#=#

main(build_data = FALSE, run_analysis = FALSE) 

