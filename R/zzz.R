# Pkg startup message
.onLoad <- function(libname, pkgname) {

  if (interactive() & getOption("jtools-update-msg", FALSE) == FALSE) {
    packageStartupMessage("The most recent jtools update (to 1.0.0) was a major",
              " update.\nPlease check out",
              " http://www.jtools.jacob-long.com/news/\n",
              "for details on what is new.\n")
  }

  if (interactive()) {
    options("jtools-update-msg" = TRUE)
  }
}
