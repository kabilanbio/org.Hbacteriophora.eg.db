\name{org.Hbacteriophora.eg.db}
\alias{org.Hbacteriophora.eg.db}
\alias{org.Hbacteriophora.eg}
\title{Bioconductor annotation data package}
\description{

  Welcome to the org.Hbacteriophora.eg.db annotation Package.  This is an
  organism specific package. The purpose is to provide detailed
  information about the species abbreviated in the second part of the
  package name org.Hbacteriophora.eg.db. This package is updated biannually.

  To learn more about this package, users are encouraged to learn about
  the select, columns, keys and keytypes methods.  These are described
  in a walkthrough on the bioconductor website as well as in the manual
  pages and vignettes in the AnnotationDbi package.
}

\examples{
library(AnnotationHub)
ah <- AnnotationHub()
orgdb <- ah[["AH121717"]]
keytypes(orgdb)
head(keys(orgdb, keytype = "KEGGPATHWAY"))
}

\keyword{datasets}

