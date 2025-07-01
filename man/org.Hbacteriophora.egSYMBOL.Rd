\name{org.Hbacteriophora.egSYMBOL}
\alias{org.Hbacteriophora.egSYMBOL}
\title{Retrieve All Gene Symbols}
\description{Gene symbols are standardized short-form identifiers used in biological studies.}
\value{A character vector of gene symbols.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
head(keys(orgdb, keytype = "SYMBOL"))
}
\keyword{datasets}
