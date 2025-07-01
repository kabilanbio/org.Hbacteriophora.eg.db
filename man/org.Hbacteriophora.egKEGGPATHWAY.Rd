\name{org.Hbacteriophora.egKEGGPATHWAY}
\alias{org.Hbacteriophora.egKEGGPATHWAY}
\title{Map Genes to KEGG Pathways}
\description{KEGG pathways represent functional modules and metabolic pathways.}
\value{A data frame mapping genes to KEGG pathways.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
gids <- head(keys(orgdb, keytype = "GID"))
select(orgdb, keys = gids, columns = "KEGGPATHWAY", keytype = "GID")
}
\keyword{datasets}
