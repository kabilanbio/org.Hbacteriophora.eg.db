\name{org.Hbacteriophora.egGOALL}
\alias{org.Hbacteriophora.egGOALL}
\title{Map Genes to GO Terms (Including Ancestors)}
\description{Retrieves all GO terms including hierarchical ancestors.}
\value{A data frame with GOALL terms, ONTOLOGYALL, and EVIDENCEALL.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
gids <- head(keys(orgdb, keytype = "GID"))
select(orgdb, keys = gids, columns = "GOALL", keytype = "GID")
}
\keyword{datasets}
