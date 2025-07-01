\name{org.Hbacteriophora.egGO}
\alias{org.Hbacteriophora.egGO}
\title{Map Genes to GO Terms (Direct Annotations)}
\description{Retrieves directly annotated Gene Ontology terms.}
\value{A data frame of GO terms, ontology, and evidence.}
\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
gids <- head(keys(orgdb, keytype = "GID"))
select(orgdb, keys = gids, columns = "GO", keytype = "GID")
}
\keyword{datasets}
