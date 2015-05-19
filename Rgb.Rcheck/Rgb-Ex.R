pkgname <- "Rgb"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('Rgb')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Annotation")
### * Annotation

flush(stderr()); flush(stdout())

### Name: Annotation
### Title: Annotation track constructors
### Aliases: Annotation track.table.GTF track.exons.CCDS track.CNV.DGV
###   track.genes.NCBI track.bands.UCSC

### ** Examples

  # From the "How-to" vignette, section "Custom annotation tracks"
  file <- system.file("extdata/Cosmic_ATM.gtf.gz", package="Rgb")
  tt <- track.table.GTF(file)



cleanEx()
nameEx("istrack_subtrack_sizetrack")
### * istrack_subtrack_sizetrack

flush(stderr()); flush(stdout())

### Name: subtrack
### Title: Extract elements within a genomic window
### Aliases: subtrack sizetrack istrack

### ** Examples

  
  # Exemplar data : subset of human genes
  data(hsFeatures)
  
  # Track validity
  print(istrack(hsGenes))
  hsGenes <- hsGenes[ order(hsGenes$chrom, hsGenes$start) ,]
  print(istrack(hsGenes))
  
  # Chromosome index (factorial 'chrom')
  index <- tapply(1:nrow(hsGenes), hsGenes$chrom, max)
  
  # Factor chrom query
  print(class(hsGenes$chrom))
  subtrack("1", 10e6, 15e6, index, hsGenes)
  
  # Row count
  a <- nrow(subtrack("1", 10e6, 15e6, index, hsGenes))
  b <- sizetrack("1", 10e6, 15e6, index, hsGenes)
  if(a != b) stop("Inconsistency")
  
  # Multiple sources
  length <- hsGenes$end - hsGenes$start
  subtrack("1", 10e6, 15e6, index, hsGenes, length)
  subtrack("1", 10e6, 15e6, index, hsGenes, length=length)
  
  # Speed comparison (x200 here)
  system.time(
    for(i in 1:40000) {
      subtrack("1", 10e6, 15e6, index, hsGenes)
    }
  )
  system.time(
    for(i in 1:200) {
      hsGenes[ hsGenes$chrom == "1" & hsGenes$start <= 15e6 & hsGenes$end >= 10e6 ,]
    }
  )
  
  # Convert chrom from factor to integer
  hsGenes$chrom <- as.integer(as.character(hsGenes$chrom))
  
  # Chromosome index (integer 'chrom')
  index <- rep(NA_integer_, 24)
  tmpIndex <- tapply(1:nrow(hsGenes), hsGenes$chrom, max)
  index[ as.integer(names(tmpIndex)) ] <- tmpIndex
  
  # Integer chrom query
  print(class(hsGenes$chrom))
  subtrack(1, 10e6, 15e6, index, hsGenes)



cleanEx()
nameEx("refTable-class")
### * refTable-class

flush(stderr()); flush(stdout())

### Name: refTable-class
### Title: Class '"refTable"'
### Aliases: refTable-class
### Keywords: classes

### ** Examples

  # New empty refTable
  tab <- new("refTable")
  tab$addColumn(1:5, "C1")
  tab$addColumn(letters[1:5], "C2")
  tab$setRowNames(LETTERS[11:15])
  
  # New filled refTable (same content)
  tab <- refTable(C1=1:5, C2=letters[1:5], row.names=LETTERS[11:15])
  
  # Whole table print
  print(tab$extract())
  
  # Data update
  tab$fill(c(2,4), 2, c("B","D"))
  
  # Data extraction
  print(tab$extract(1:3))
  print(tab$extract(c(TRUE, FALSE)))
  print(tab$extract("K", "C1"))
  
  # Expression-based extraction
  expr <- expression(C1 %% 2 == 1)
  print(tab$extract(expr))
  
  # Table extension
  tab$addEmptyRows(5L, LETTERS[1:5])
  tab$fill(6:10, "C1", 6:10)
  print(tab$extract())
  
  # Filling from R objects
  tab <- new("refTable")
  print(tab$extract())
  tab$addVectors(C1=1:5, C2=letters[1:5])
  print(tab$extract())
  tab$addList(list(C1=6:8, C3=LETTERS[6:8]))
  print(tab$extract())
  
  # Beware of recycling !
  tab$addVectors(C1=9:15, C3=LETTERS[9:10])
  print(tab$extract())



cleanEx()
nameEx("refTable-constructor")
### * refTable-constructor

flush(stderr()); flush(stdout())

### Name: refTable-constructor
### Title: refTable class constructor
### Aliases: refTable

### ** Examples

  # From vectors
  tab <- refTable(colA=1:5, colB=letters[1:5])
  print(tab$extract(3,))
  
  # From list (recycling)
  columns <- list(number=1, letters=LETTERS)
  tab <- refTable(columns)
  print(tab$extract())
  
  # data.frame conversion
  dataFrame <- data.frame(colA=1:5, colB=letters[1:5])
  tab <- refTable(dataFrame)
  print(tab$extract())



cleanEx()
nameEx("track-constructors")
### * track-constructors

flush(stderr()); flush(stdout())

### Name: track-constructors
### Title: Track constructors
### Aliases: track-constructors track.table track.bam track.genes
###   track.bands track.exons track.CNV

### ** Examples

  # track.table from a data.frame
  df <- data.frame(chrom=1, strand="+", start=1:5, end=2:6, name=letters[1:5], stringsAsFactors=FALSE)
  track.table(df)
  
  # track.table from vectors
  track.table(chrom=1, strand="+", start=1:5, end=2:6, name=letters[1:5])
  
  # track.bam
  track.bam(system.file("extdata/ATM.bam", package="Rgb"))



cleanEx()
nameEx("track.table-class")
### * track.table-class

flush(stderr()); flush(stdout())

### Name: track.table-class
### Title: Class '"track.table"'
### Aliases: track.table-class
### Keywords: classes

### ** Examples

  # Exemplar data : subset of human genes
  data(hsFeatures)
  
  # Construction
  trackTable <- track.table(
    hsGenes,
    .name = "NCBI Genes",
    .organism = "Homo sapiens",
    .assembly = "GRCh37"
  )
  
  # Slicing
  print(trackTable$slice(chrom="1", as.integer(15e6), as.integer(20e6)))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
