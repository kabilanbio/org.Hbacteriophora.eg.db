\name{org.Hbacteriophora.egONTOLOGYALL}
\alias{org.Hbacteriophora.egONTOLOGYALL}
\title{List GO Ontology Categories (All Terms)}
\description{Ontology types associated with all GO terms (direct + ancestors).}
\value{A character vector of ontology types.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
gids <- head(keys(orgdb, keytype = "GID"))
select(orgdb, keys = gids, columns = "ONTOLOGYALL", keytype = "GID")
}
\keyword{datasets}
