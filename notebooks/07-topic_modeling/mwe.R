library(quanteda)
library(topicmodels)
suppressPackageStartupMessages(library(stm))

data("data_corpus_irishbudget2010", package = "quanteda.textmodels")
dtm <- dfm(tokens(data_corpus_irishbudget2010, remove_punct = TRUE))

model <- LDA(dtm, method = "Gibbs", k = 10, control = list(seed = 1234))
model <- stm(dtm, K = 10, max.em.its = 20, control=list(alpha=1), verbose=FALSE)



