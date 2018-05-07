# Pkg startup message
.onAttach <- function(libname, pkgname) {

  # Thanks to G. Grothendieck for this idea for giving a limited time
  # message after installation
  # https://stackoverflow.com/questions/40748744/r-package-with-code-that-only
  # -runs-once-per-installation

  if (interactive()) {
    ctime <- file.info(find.package(pkgname))$ctime
    days <- as.integer(round(difftime(Sys.time(), ctime, units = "day"), 0))
  } else {days <- 4}
  if (days < 3) {
    msg <- wrap_str("The most recent jtools update (to 1.0.0) was a major
    update. Please check out http://www.jtools.jacob-long.com/news/ for
    details on what is new. This message will go away in ", 3 - days, " days.")
    packageStartupMessage(msg)
  }

}
