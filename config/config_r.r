#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# R Configuration
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ADD VERSION NUMBERS OF PACKAGES

# Add required packages from CRAN to this vector
CRAN_packages <- c(
  "haven",         # read and write .dta files
  "arrow",         # read and write parquet files
  "tidyverse",     # data manipulation and visualization
  
  "lfe",           # estimate fixed effects models efficiently
  "sandwich",      # robust standard errors
  "lmtest",        # hypothesis testing for linear models
  "stargazer",     # regression tables

  # "geodist",       # fast geographic distance calculations
  "remotes",       # install specific package versions from CRAN
  # "USAboundaries", # US geographic boundaries
  "tigris",        # US geographic boundaries
  "lwgeom",        # US spatial operations (compute area)
  "sf",            # spatial data handling
  "binsreg",       # binscatter plots
  "telegram.bot"   # send notifications once code has finished running
)

# Pin versions for some CRAN packages (name = "version")
CRAN_versions <- c(
  "dplyr" = "1.1.4", 
  "ggplot2" = "3.5.1"
  )

# GitHub packages
GitHub_packages <- c(
  "louisheizlsperger/labelstar"
)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# INSTALL/LOAD HELPERS
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

ensure_pkg <- function(pkg, repos, dependencies = TRUE, quiet = FALSE) {
  if (!is_installed(pkg)) {
    install.packages(pkg, repos = repos, dependencies = dependencies, quiet = quiet)
  }
}

install_cran_pkg <- function(pkg, repos, dependencies, quiet, upgrade, versions_named) {
  want_version <- length(versions_named) > 0 && !is.na(versions_named[ pkg ]) && nzchar(versions_named[ pkg ])
  if (want_version) {
    # Install a specific version via 'remotes'
    if (!is_installed("remotes")) {
      install.packages("remotes", repos = repos, dependencies = TRUE, quiet = quiet)
    }
    remotes::install_version(pkg,
                             version      = versions_named[ pkg ],
                             repos        = repos,
                             dependencies = dependencies,
                             upgrade      = if (upgrade) "always" else "never",
                             quiet        = quiet)
  } else {
    if (upgrade || !is_installed(pkg)) {
      install.packages(pkg, repos = repos, dependencies = dependencies, quiet = quiet)
    }
  }
}

normalize_github_spec <- function(x) {
  # strip leading GitHub URL and trailing .git
  x <- sub("^https?://github.com/", "", x)
  x <- sub("\\.git$", "", x)
  x
}

repo_to_pkgname <- function(spec) {
  # last path segment is usually the package name
  spec <- normalize_github_spec(spec)
  segs <- strsplit(spec, "/")[[1]]
  segs[length(segs)]
}

install_github_pkg <- function(spec, upgrade, quiet, repos) {
  if (!is_installed("devtools")) {
    install.packages("devtools", repos = repos, dependencies = TRUE, quiet = quiet)
  }
  spec2 <- normalize_github_spec(spec)
  devtools::install_github(spec2, upgrade = upgrade)
}

safe_library <- function(pkg, quiet = FALSE) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE, quietly = quiet, warn.conflicts = FALSE)
  )
}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# MAIN ENTRY
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

main <- function(CRAN_packages = NULL, GitHub_packages = NULL,
                 CRAN_repo = "https://cran.r-project.org",
                 dependencies = TRUE, quiet = FALSE, upgrade = FALSE,
                 CRAN_versions = NULL) {
  
  # CRAN install (with optional pinning)
  if (!is.null(CRAN_packages) && length(CRAN_packages) > 0) {
    for (pkg in CRAN_packages) {
      install_cran_pkg(pkg,
                       repos        = CRAN_repo,
                       dependencies = dependencies,
                       quiet        = quiet,
                       upgrade      = upgrade,
                       versions_named = CRAN_versions)
    }
  }
  
  # GitHub install
  if (!is.null(GitHub_packages) && length(GitHub_packages) > 0) {
    # devtools needed only if GitHub packages provided
    ensure_pkg("devtools", repos = CRAN_repo, dependencies = TRUE, quiet = quiet)
    for (spec in GitHub_packages) {
      install_github_pkg(spec, upgrade = upgrade, quiet = quiet, repos = CRAN_repo)
    }
  }
  
  # Load CRAN packages
  if (!is.null(CRAN_packages) && length(CRAN_packages) > 0) {
    for (pkg in CRAN_packages) {
      safe_library(pkg, quiet = quiet)
    }
  }
  
  # Load GitHub packages (by repo name)
  if (!is.null(GitHub_packages) && length(GitHub_packages) > 0) {
    for (spec in GitHub_packages) {
      pkgname <- repo_to_pkgname(spec)
      safe_library(pkgname, quiet = quiet)
    }
  }
}

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# EXECUTE ON SOURCE
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

main(CRAN_packages, GitHub_packages,
     CRAN_repo = "https://cran.r-project.org",
     dependencies = TRUE, quiet = FALSE, upgrade = FALSE,
     CRAN_versions = CRAN_versions)


#=#=#=#=#=#=#

# Telegram notification bot setup
renviron_file <- file.path(config, ".Renviron")
readRenviron(renviron_file)

# Telegram (owner-only via TELEGRAM_NOTIFY)
want  <- Sys.getenv("TELEGRAM_NOTIFY") == "1"
token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
chat  <- Sys.getenv("TELEGRAM_CHAT_ID")

bot <- if (want && nzchar(token) && nzchar(chat)) telegram.bot::Bot(token = token) else NULL

notify <- function(text) {
  if (!is.null(bot)) bot$sendMessage(chat_id = chat, text = text)
}
