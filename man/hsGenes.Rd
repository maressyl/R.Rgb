\name{hsFeatures}
\docType{data}
\alias{hsFeatures}
\alias{hsGenes}
\alias{hsBands}
\title{Homo sapiens GRCh37 genomic features}
\description{
  \code{hsGenes} contains 8000 randomly choosen human genes, from the NCBI repository.
  
  \code{hsBands} contains the whole list of human chromosome G-banding, from the UCSC repository.
  
  Both are provided as exemplar data of genomic features.
}
\usage{
  hsGenes
  hsBands
}
\format{
  For \code{hsGenes}, a data.frame with 8000 rows and the following columns : "chrom", "start", "end" and "name".
  
  For \code{hsBands}, a track.table object with 862 rows and the following columns : "name", "chrom", "strand", "start", "end", "stain".
}
\source{
  National Center for Biotechnology Information (ftp.ncbi.nih.gov)
  
  University of California, Santa Cruz (genome.ucsc.edu)
}
\keyword{datasets}
