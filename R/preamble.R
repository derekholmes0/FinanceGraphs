#Preamble

the_fg <- new.env(parent = emptyenv())
the_fg$cachedir <- tools::R_user_dir("FinanceGraphs", which = "cache")
load("./R/sysdata.rda",envir=the_fg)
#  loads tevents_defaults and ratingsmapmelt
the_fg$doifn <- paste0( the_fg$cachedir, "/fg_doi.RD")
the_fg$aesfn <- paste0( the_fg$cachedir, "/fg_aes.RD")
the_fg$themefn <- paste0( the_fg$cachedir, "/fg_theme.RD")
the_fg$doi_dates <-  the_fg$doi_default
the_fg$aesset <- the_fg$aes_default
the_fg$curr_theme <- the_fg$theme_default
the_fg$gpname <- NULL
the_fg$verbose <- FALSE
the_fg$cassign <- FALSE

if(file.exists(the_fg$doifn)) {
  load(the_fg$doifn)
  the_fg$doi_dates <- newdoi
}
if(file.exists(the_fg$aesfn)) {
  load(the_fg$aesfn)
  the_fg$aesset <- newaes
}
if(file.exists(the_fg$themefn)) {
  load(the_fg$themefn)
  the_fg$curr_theme <- newTheme
}

.datatable.aware = TRUE

