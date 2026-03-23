#Preamble

the <- new.env(parent = emptyenv())
the$cachedir <- tools::R_user_dir("FinanceGraphs", which = "cache")
load("./R/sysdata.rda",envir=the)
#  loads tevents_defaults and ratingsmapmelt
the$doifn <- paste0( the$cachedir, "/fg_doi.RD")
the$aesfn <- paste0( the$cachedir, "/fg_aes.RD")
the$themefn <- paste0( the$cachedir, "/fg_theme.RD")
the$doi_dates <-  the$doi_default
the$aesset <- the$aes_default
the$curr_theme <- the$theme_default
the$gpname <- NULL
the$verbose <- FALSE
the$cassign <- FALSE

if(file.exists(the$doifn)) {
  load(the$doifn)
  the$doi_dates <- newdoi
}
if(file.exists(the$aesfn)) {
  load(the$aesfn)
  the$aesset <- newaes
}
if(file.exists(the$themefn)) {
  load(the$themefn)
  the$curr_theme <- newTheme
}

.datatable.aware = TRUE

