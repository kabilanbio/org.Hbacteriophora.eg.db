# zzz.R

datacache <- new.env(hash = TRUE, parent = emptyenv())

org.Hbacteriophora.eg <- function() {
  AnnotationDbi::showQCData("org.Hbacteriophora.eg", datacache)
}

org.Hbacteriophora.eg_dbconn <- function() {
  AnnotationDbi::dbconn(datacache)
}

org.Hbacteriophora.eg_dbfile <- function() {
  AnnotationDbi::dbfile(datacache)
}

org.Hbacteriophora.eg_dbschema <- function(file = "", show.indices = FALSE) {
  AnnotationDbi::dbschema(datacache, file = file, show.indices = show.indices)
}

org.Hbacteriophora.eg_dbInfo <- function() {
  AnnotationDbi::dbInfo(datacache)
}

org.Hbacteriophora.egORGANISM <- "Heterorhabditis bacteriophora"

.onLoad <- function(libname, pkgname) {
  # Do nothing — database will be loaded from AnnotationHub
  invisible()
}

.onUnload <- function(libpath) {
  # Optional: clean up dbconn if used in manual mode
  if (exists("dbconn", envir = datacache)) {
    conn <- get("dbconn", envir = datacache)
    if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn)
  }
}
