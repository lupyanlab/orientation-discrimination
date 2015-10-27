library(plyr)
library(dplyr)

compile <- function(dir, key, headername) {
  fnames <- list.files(dir, key, full.names=TRUE)
  header <- read.table(paste0(dir,'/',headername), header=T, sep='\t')
  header <- names(header)
  ldply(fnames, read.table, sep='\t', header=F, col.names=header, 
        row.names=NULL, stringsAsFactors=F)
}
