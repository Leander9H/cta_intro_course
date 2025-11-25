# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Examining difference in UK newspapers' immigration stances
#' @author Hauke Licht
#' @date   2025-11-18
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ---- 
library(quanteda)
library(quanteda.corpora)  # TODO (if needed): `renv::install("quanteda/quanteda.corpora")``
library(quanteda.textmodels) # TODO (if needed): `renv::install("quanteda.textmodels")`
library(quanteda.textplots) # TODO (if needed): `renv::install("quanteda.textplots")`
library(dplyr)
library(ggplot2)

# load and prepare the data ----

# NOTE: we use the "Immigration News" corpus that contains 
#  UK news articles (2,833) from 2014 that mention immigration
data("data_corpus_immigrationnews", package = "quanteda.corpora")

# inspect the document variables
glimpse(docvars(data_corpus_immigrationnews))

# how many articles per newspaper
docvars(data_corpus_immigrationnews) |> 
  count(paperName) |> 
  arrange(desc(n))

# NOTE: remove "the-sunday-telegraph" given very few articles
data_corpus_immigrationnews <- corpus_subset(data_corpus_immigrationnews, paperName != "the-sunday-telegraph")

# create the document-term matrix ----

dtm <- data_corpus_immigrationnews |> 
  tokens(remove_symbols = TRUE, remove_numbers = TRUE) |> 
  tokens_tolower() |> 
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  tokens(remove_punct = TRUE) |> 
  tokens_remove(stopwords("en")) |>
  dfm() |> 
  dfm_trim(
    min_termfreq = 20, termfreq_type = "count",
    max_docfreq = 0.80, docfreq_type = "prop"
  )

sparsity(dtm)

# aggregate the data by news paper ----

# NOTE: the news articles are too sparse and its too much data to efficiently fit
#        a Wordfish model Therefore, we aggregate the data by newspaper
dtm_grouped <- dfm_group(dtm, group = docvars(dtm, "paperName"))
sparsity(dtm_grouped)

# fit the Wordfish model ----

# NOTE: let's assume that The Guardian < The Daily Mail on the latent dimension
#        This makes sense because The Guardian is known to be more liberal 
#        whereas the Daily Mail is known to be more conservative tabloid media
# get the indexes of these news paper's in the DTM
low_ <- which(docid(dtm_grouped) == "guardian") 
high_ <- which(docid(dtm_grouped) == "mail")

# fit the model
wf_papers <- textmodel_wordfish(dtm_grouped, dir = c(low_, high_))

# inspect the estimate
summary(wf_papers)

# plot the position estimates
textplot_scale1d(wf_papers, "documents")
# NOTE: we can interpret these results as follows:
#  - given is the constraint that The Guardian < The Daily Mail
#  - so higher values likely indicate more conservative/restrictive
#     stance on immigration/portrayal of immigrants
#  - but The Sun and The Express turn out to be even more "right" than 
#     the Daily Mail
#  - lower values likely correspond to more liberal/permissive 
#     immigration stance

# NOTE: the assumption that the estimated scale captures (mostly) immigration
#  stance would need to be further evaluatee, e.g., by correlating Wordfish 
#  scores with external measures

# OPTIONAL: analyze news papers' articles weekly----

#' This setup is typical in so-called time-series cross-section analysis 
#'  where you have repeated measurements over time (i.e., a "time series") 
#'  for several unit of interst (e.g., parties or, here, newspapers)
range(as.integer(docvars(data_corpus_immigrationnews, "day")))

# get calendar week from day number
day_numbers <- as.integer(docvars(dtm, "day"))
hist(day_numbers) # NOTE: theoretically, these values are in 1-365 range
docvars(dtm, "week") <- as.integer(format(as.Date(day_numbers, origin = "2014-01-01"), format = "%V"))

# create news paper X week ID
docvars(dtm, "paper_week") <- with(docvars(dtm), interaction(paperName, week, sep = "_week"))

# aggregate data at newspaper X week level
dtm_papers_weeks <- dfm_group(dtm, group = docvars(dtm, "paper_week")) 

# get reference indexes for The Guardian and The Daily Mail
low_ <- grep("guardian_", docnames(dtm_papers_weeks))[1]
high_ <- grep("mail_", docnames(dtm_papers_weeks))[1]

# fit
wf_papers_weeks <- textmodel_wordfish(dtm_papers_weeks, dir = c(low_, high_))
summary(wf_papers_weeks)

# let's get the position estimates
thetas <- as.data.frame(coef(wf_papers_weeks, "documents"))
thetas["theta_se"] <- wf_papers_weeks$se.theta
thetas["doc_id"] <-  rownames(thetas)
# join them with other document (i.e., news paper X week level units) metadata
papers_weeks_dovars <- docvars(dtm_papers_weeks)
papers_weeks_dovars["doc_id"] <- docnames(dtm_papers_weeks)
thetas <- left_join(thetas, papers_weeks_dovars, by = "doc_id")

# create time-series plots
ggplot(
  data = thetas, 
  mapping = aes(
    x = week, 
    y = theta, ymin = theta - 1.96 * theta_se, ymax = theta + 1.96 * theta_se,
    group = paperName,
    color = paperName
  )
) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(alpha = 0.2, show.legend = FALSE) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

'ä