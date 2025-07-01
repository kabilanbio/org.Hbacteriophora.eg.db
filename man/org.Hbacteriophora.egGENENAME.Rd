\name{org.Hbacteriophora.egGENENAME}
\alias{org.Hbacteriophora.egGENENAME}
\title{Retrieve Gene Names for Given Symbols}
\description{Provides full gene names for given gene symbols.}
\value{A data frame of gene symbols and full gene names.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
symbols <- head(keys(orgdb, keytype = "SYMBOL"))
select(orgdb, keys = symbols, columns = "GENENAME", keytype = "SYMBOL")
}
\keyword{datasets}
