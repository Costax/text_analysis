#
# Author: Adan Hirales Carbajal
# Email: adan.hirales@cetys.mx
#

rule_generator <- function( ) {

    # Get command line arguments
    args = commandArgs(trailingOnly=TRUE)

    # Parse arguments (basename & dirname)
    inFile      <- args[1]
    _sup        <- args[2]
    _conf       <- args[3]
    _minlen     <- args[4]

    docCorpus <- read.transactions(inFile, sep=",")
    ruleSet <- apriori(docCorpus, parameter = list(support=_sup, confidence=_conf, minlen=_minlen)) 

    # Select the dominant rules
    filteredRuleSet <- head(sort(ruleSet,by="lift"),100)
    # Extract the rules
    rules <- inspect(ruleSet)
    plot(filteredRuleSet, method=NULL, measure="support", shading="lift", interactive=FALSE)
}

library(arules, quietly = TRUE)
library(arulesViz, quietly = TRUE)

rule_generator()
