datacache <- new.env(hash = TRUE, parent = emptyenv())

#' @title Access Heterorhabditis bacteriophora annotation data
#' @export
org.Hbacteriophora.eg <- function() AnnotationDbi::showQCData("org.Hbacteriophora.eg", datacache)

#' @title Get database connection
#' @export
org.Hbacteriophora.eg_dbconn <- function() AnnotationDbi::dbconn(datacache)

#' @title Get database file path
#' @export
org.Hbacteriophora.eg_dbfile <- function() AnnotationDbi::dbfile(datacache)

#' @title Get database schema
#' @export
org.Hbacteriophora.eg_dbschema <- function(file = "", show.indices = FALSE) {
  AnnotationDbi::dbschema(datacache, file = file, show.indices = show.indices)
}

#' @title Get database info
#' @export
org.Hbacteriophora.eg_dbInfo <- function() AnnotationDbi::dbInfo(datacache)

# Package metadata
org.Hbacteriophora.egORGANISM <- "Heterorhabditis bacteriophora"

.onLoad <- function(libname, pkgname) {
  if (nzchar(Sys.getenv("R_INSTALL_PKG")) || nzchar(Sys.getenv("R_PACKAGE_BUILDING"))) {
    assign("dbfile", "", envir = datacache)
    return(invisible())
  }

  # 1. Use bundled database if exists
  dbfile <- system.file("extdata", "org.Hbacteriophora.eg.sqlite",
                        package = pkgname, lib.loc = libname)

  # 2. Define persistent local cache path
  cache_dir <- tools::R_user_dir(pkgname, which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  persistent_dbfile <- file.path(cache_dir, "org.Hbacteriophora.eg.sqlite")

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

  # 3. If bundled DB is invalid, check or download to persistent location
  if (!is_db_valid(dbfile)) {
    dbfile <- persistent_dbfile

    zenodo_url <- "https://zenodo.org/records/15692332/files/org.Hbacteriophora.eg.sqlite"

    if (!is_db_valid(dbfile)) {
      message("Downloading H. bacteriophora annotation database (once per system)...")

      tryCatch({
        if (requireNamespace("curl", quietly = TRUE)) {
          curl::curl_download(
            zenodo_url,
            dbfile,
            mode = "wb",
            handle = curl::new_handle(CONNECTTIMEOUT = 300, TIMEOUT = 600)
          )
        } else {
          options(timeout = max(6000000, getOption("timeout")))
          utils::download.file(zenodo_url, dbfile, mode = "wb", quiet = FALSE)
        }

        if (!is_db_valid(dbfile)) {
          file.remove(dbfile)
          stop("Downloaded database is invalid.")
        }
      }, error = function(e) {
        if (file.exists(dbfile)) file.remove(dbfile)
        stop("Failed to download database. You can:\n",
             "1. Try again later when you have better internet connection\n",
             "2. Manually download from:\n", zenodo_url, "\n",
             "   and place it in:", cache_dir,
             call. = FALSE)
      })
    }
  }

  # 4. Initialize connection
  tryCatch({
    assign("dbfile", dbfile, envir = datacache)
    dbconn <- AnnotationDbi::dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir = datacache)

    db <- AnnotationDbi::loadDb(dbfile, packageName = pkgname)
    dbNewname <- AnnotationDbi:::dbObjectName(pkgname, "OrgDb")
    ns <- asNamespace(pkgname)
    assign(dbNewname, db, envir = ns)
    namespaceExport(ns, dbNewname)

    packageStartupMessage(
      sprintf("%s.db loaded successfully\nSource: %s",
              sub(".db$", "", pkgname),
              ifelse(grepl("cache", dbfile), "Zenodo cache", "bundled package"))
    )
  }, error = function(e) {
    stop("Failed to initialize database: ", conditionMessage(e), call. = FALSE)
  })
}

.onUnload <- function(libpath) {
  if (exists("dbconn", envir = datacache)) {
    conn <- get("dbconn", envir = datacache)
    if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn)
  }
  # Clean up temp file if it exists
  if (exists("dbfile", envir = datacache)) {
    dbfile <- get("dbfile", envir = datacache)
    if (grepl("temp", dbfile) && file.exists(dbfile)) {
      file.remove(dbfile)
    }
  }
}
