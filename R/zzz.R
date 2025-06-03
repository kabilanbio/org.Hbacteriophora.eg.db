datacache <- new.env(hash=TRUE, parent=emptyenv())

org.Hbacteriophora.eg <- function() showQCData("org.Hbacteriophora.eg", datacache)
org.Hbacteriophora.eg_dbconn <- function() dbconn(datacache)
org.Hbacteriophora.eg_dbfile <- function() dbfile(datacache)
org.Hbacteriophora.eg_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache, file=file, show.indices=show.indices)
org.Hbacteriophora.eg_dbInfo <- function() dbInfo(datacache)

org.Hbacteriophora.egORGANISM <- "Heterorhabditis bacteriophora"

.onLoad <- function(libname, pkgname) {
  # Location of bundled DB
  dbfile <- system.file("extdata", "org.Hbacteriophora.eg.sqlite", package = pkgname, lib.loc = libname)

  # Zenodo URL for downloading DB if missing or corrupted
  zenodo_url <- "https://zenodo.org/records/15579729/files/org.Hbacteriophora.eg.sqlite"

  # Helper to check DB validity (simple integrity check)
  is_db_valid <- function(dbf) {
    if (!file.exists(dbf)) return(FALSE)
    con <- NULL
    valid <- FALSE
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), dbf)
      res <- DBI::dbGetQuery(con, "PRAGMA integrity_check;")
      valid <- (is.data.frame(res) && nrow(res) == 1 && res$integrity_check == "ok")
    }, error = function(e) {
      valid <<- FALSE
    }, finally = {
      if (!is.null(con)) DBI::dbDisconnect(con)
    })
    valid
  }

  # If bundled DB is missing or invalid, download it to a writable cache directory
  if (!is_db_valid(dbfile)) {
    cache_dir <- rappdirs::user_cache_dir("org.Hbacteriophora.eg")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    dbfile_cache <- file.path(cache_dir, "org.Hbacteriophora.eg.sqlite")

    if (!is_db_valid(dbfile_cache)) {
      message("SQLite database not found or invalid. Downloading from Zenodo...")
      tryCatch({
        utils::download.file(zenodo_url, destfile = dbfile_cache, mode = "wb")
        message("Download completed.")
      }, error = function(e) {
        warning("Failed to download SQLite database: ", conditionMessage(e))
      })
    }
    dbfile <- dbfile_cache
  }

  assign("dbfile", dbfile, envir = datacache)
  dbconn <- dbFileConnect(dbfile)
  assign("dbconn", dbconn, envir = datacache)

  sPkgname <- sub(".db$", "", pkgname)
  db <- loadDb(dbfile, packageName = pkgname)
  dbNewname <- AnnotationDbi:::dbObjectName(pkgname, "OrgDb")
  ns <- asNamespace(pkgname)
  assign(dbNewname, db, envir = ns)
  namespaceExport(ns, dbNewname)

  packageStartupMessage(AnnotationDbi:::annoStartupMessages("org.Hbacteriophora.eg.db"))
}

.onUnload <- function(libpath)
{
  dbFileDisconnect(org.Hbacteriophora.eg_dbconn())
}

