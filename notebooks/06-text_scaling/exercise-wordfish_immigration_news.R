# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Examining difference in UK newspapers' immigration stances
#' @author Hauke Licht
#' @date   2025-11-18
#' @note   look for keyword `TODO` to see which steps you need to complete
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ---- 
library(quanteda)
library(quanteda.corpora)  # if needed: `renv::install("quanteda/quanteda.corpora")``
library(quanteda.textmodels) # if needed: `renv::install("quanteda.textmodels")`
library(quanteda.textplots) # if needed: `renv::install("quanteda.textplots")`
library(dplyr)
library(ggplot2)

# load and prepare the data ----

# NOTE: we use the "Immigration News" corpus that contains UK news articles 
#        from 2014 that mention immigration
data("data_corpus_immigrationnews", package = "quanteda.corpora")

# inspect the document variables
glimpse(docvars(data_corpus_immigrationnews))

# how many articles per newspaper
docvars(data_corpus_immigrationnews) |> 
  count(paperName) |> 
  arrange(desc(n))

# NOTE: remove "the-sunday-telegraph" given very few articles
data_corpus_immigrationnews <- corpus_subset(data_corpus_immigrationnews, paperName != "the-sunday-telegraph")

# TODO: create the document-term matrix
#  - remove symbols, numbers, and punctuation
#  - convert to lowercase
#  - create n-grams (1 to 3)
#  - lemmatize the tokens
#  - remove stopwords
#  - trim the DTM to remove very infrequent and very frequent terms
#  - IMPORTANT: name the resulting object "dtm"

# aggregate the data by news paper
# NOTE: the news articles are too sparse and its too much data to efficiently fit
#        a Wordfish model Therefore, we aggregate the data by newspaper
dtm_grouped <- dfm_group(dtm, group = docvars(dtm, "paperName"))

# NOTE: let's assume that The Guardian < The Daily Mail on the latent dimension
#        This makes sense because The Guardian is known to be more liberal 
#        whereas the Daily Mail is known to be more conservative tabloid media
# get the indexes of these news papers' in the DTM
low_ <- which(docid(dtm_grouped) == "guardian") 
high_ <- which(docid(dtm_grouped) == "mail")

# TODO: fit the model
#  - IMPORTANT: name the resulting object "wf_papers"
wf_papers <- textmodel_wordfish(dtm_grouped, dir = c(low_, high_))

# inspect the estimate
summary(wf_papers)

# TODO: plot the position estimates using `textplot_scale1d`
textplot_scale1d(wf_papers, "documents")
# TODO: interpret the plot
#  - what might high scores indicate?
#  - which newspapers are more "right" or "left" on the estimated scale?

# OPTIONAL: analyze news papers' articles by weekly intervals ----

# NOTE: This setup is typical in so-called time-series cross-section analysis 
#        where you have repeated measurements over time (i.e., a "time series") 
#        for several unit of interst (e.g., parties or, here, newspapers)
range(as.integer(docvars(data_corpus_immigrationnews, "day")))

# get calendar week from day number
day_numbers <- as.integer(docvars(dtm, "day"))
hist(day_numbers) # NOTE: theoretically, these values are in 1-365 range
docvars(dtm, "week") <- as.integer(format(as.Date(day_numbers, origin = "2014-01-01"), format = "%V"))

# create news paper X week ID
docvars(dtm, "paper_week") <- with(docvars(dtm), interaction(paperName, week, sep = "_week"))

# TODO: aggregate  at newspaper X week level using the "paper_week" indciator
#  - IMPORTANT: name the resulting object "dtm_papers_weeks"

# get reference indexes for The Guardian and The Daily Mail
low_ <- grep("guardian_", docnames(dtm_papers_weeks))[1]
high_ <- grep("mail_", docnames(dtm_papers_weeks))[1]

# TODO: fit the model
#  - IMPORTANT: name the resulting object "wf_papers_weeks"

# let's get the position estimates
thetas <- as.data.frame(coef(wf_papers_weeks, "documents"))
thetas["theta_se"] <- wf_papers_weeks$se.theta
thetas["doc_id"] <-  rownames(thetas)
# join them with other document (i.e., news paper X week level units) metadata
papers_weeks_dovars <- docvars(dtm_papers_weeks)
papers_weeks_dovars["doc_id"] <- docnames(dtm_papers_weeks)
thetas <- left_join(thetas, papers_weeks_dovars, by = "doc_id")

# TODO: create time-series plots of the estimated positions for each newspaper
#  - use `ggplot2` for plotting
#  - show weeks on the x-axis and estimated positions on the y-axis
#  - show estimates for different newspapers with different colors
