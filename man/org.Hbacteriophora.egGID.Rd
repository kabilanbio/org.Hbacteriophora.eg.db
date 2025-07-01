\name{org.Hbacteriophora.egGID}
\alias{org.Hbacteriophora.egGID}
\title{Retrieve All Internal Gene IDs (GID)}
\description{Internal database-assigned gene IDs used for consistent internal referencing.}
\value{A character vector of gene IDs.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
head(keys(orgdb, keytype = "GID"))
}
\keyword{datasets}
