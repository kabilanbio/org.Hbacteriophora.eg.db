## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(org.Hbacteriophora.eg.db)

## ----results='markup'---------------------------------------------------------
columns(org.Hbacteriophora.eg.db)  # List available fields

## -----------------------------------------------------------------------------
keys <- head(keys(org.Hbacteriophora.eg.db, keytype="GID"))  # Get sample gene IDs

## -----------------------------------------------------------------------------
select(org.Hbacteriophora.eg.db, keys=keys, columns=c("GENENAME", "SYMBOL"), keytype="GID")

## -----------------------------------------------------------------------------
select(org.Hbacteriophora.eg.db, keys=keys, columns=c("GO", "ONTOLOGY"), keytype="GID")

## -----------------------------------------------------------------------------
sessionInfo()

