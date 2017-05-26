#
# Author: Adan Hirales Carbajal
# Email: adan.hirales@cetys.mx
#

plot_generator <- function( freq = 10 ) {

    # Get command line arguments
    args = commandArgs(trailingOnly=TRUE)

    # Parse arguments (basename & dirname)
    inFile <- args[1]
    path <- args[2]
    outFile <- basename(inFile)
    outFile <- strsplit(outFile, "\\.")[[1]]
    outFile <- paste(outFile[1],"png",sep=".")

    # Load stopwords and text
    textCorpus <- readLines(inFile) 
    
    # Apply filters 
    textCorpus <- Corpus(VectorSource(textCorpus))
    textCorpus <- tm_map(textCorpus,stripWhitespace)
    textCorpus <- tm_map(textCorpus,removePunctuation)
    textCorpus <- tm_map(textCorpus,content_transformer(tolower))
    #textCorpus <- tm_map(textCorpus,removeWords,stopWords[[1]])
    textCorpusMtz <-DocumentTermMatrix(textCorpus)

    # Initiate redering process
    cwd <- getwd()
    setwd(path)
    png(outFile, width=12, height=8, units="in", res=300)
    wordcloud(textCorpus, min.freq = freq, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
    dev.off()
    setwd(cwd)
}

library(tm, quietly = TRUE)
library(wordcloud, quietly = TRUE)

plot_generator(50)
