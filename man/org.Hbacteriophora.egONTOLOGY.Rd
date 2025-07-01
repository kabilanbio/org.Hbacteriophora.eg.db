\name{org.Hbacteriophora.egONTOLOGY}
\alias{org.Hbacteriophora.egONTOLOGY}
\title{List GO Ontology Categories (Direct)}
\description{Ontology categories for directly assigned GO terms (BP, MF, CC).}
\value{A character vector of ontology types.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
gids <- head(keys(orgdb, keytype = "GID"))
select(orgdb, keys = gids, columns = "ONTOLOGY", keytype = "GID")
}
\keyword{datasets}
