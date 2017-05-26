#
# Author: Adan Hirales Carbajal
# Email: adan.hirales@cetys.mx
#

text_filter <- function( freq = 10 ) {

    # Get command line arguments
    args = commandArgs(trailingOnly=TRUE)

    # Parse arguments (basename & dirname)
    inFile <- args[1]
    inFileSW <- args[2]
    path <- args[3]

    baseFile <- basename(inFile)
    outFile <- strsplit(baseFile, "\\.")[[1]]
    outFile <- paste(outFile[1],"png",sep=".")
    tmpFile <- paste(path,baseFile,sep="")

    # Load stopwords and text
    textCorpus <- readLines(inFile) 
    stopWords <- read.csv(inFileSW)
    
   # Apply filters 
    textCorpus <- Corpus(VectorSource(textCorpus))
    textCorpus <- tm_map(textCorpus,stripWhitespace)
    textCorpus <- tm_map(textCorpus,removePunctuation)
    textCorpus <- tm_map(textCorpus,content_transformer(tolower))
    textCorpus <- tm_map(textCorpus,removeWords,stopWords[[1]])
    textCorpusMtz <-DocumentTermMatrix(textCorpus) 

    # Almacena los datos filtrados
    fileConn<-file(tmpFile)
    writeLines(textCorpus$content, fileConn)
    close(fileConn)
}

library(tm, quietly = TRUE)
library(wordcloud, quietly = TRUE)

text_filter(10)
