# ----- determine if we are local or not  ----- 

is_cran <- Sys.getenv("NOT_CRAN")

if (is_cran %in% "false") {
  skip_on_cran()
}

# ----- set timezone -----
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

# ----- set deterector ---
tests.home <- getwd()
setwd(tempdir())


  