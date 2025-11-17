# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Compare Wordfish estimates to human-coded sentiment estimates
#' @author Hauke Licht
#' @date   2025-11-18
#' @note   look for keyword `TODO` to see which steps you need to complete
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(quanteda.corpora)
library(quanteda.textmodels) # if needed `renv::install("quanteda.textmodels")`
library(quanteda.textplots) # if needed `renv::install("quanteda.textplots")`
library(readr)
library(dplyr)
library(ggplot2)

# load and prepare the data ----

# NOTE: assuming you run this file in th context of our courses R project
fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_sentiment.csv")

# load the CSV file
df <- read_csv(fp)

df <- rename(df, sentiment = label)

glimpse(df)

# prepare the document-term matrix ----

# TODO: create the document-term matrix
#  - remove symbols, numbers, and punctuation
#  - convert to lowercase
#  - create n-grams (1 to 3)
#  - lemmatize the tokens
#  - remove stopwords
#  - trim the DTM to remove very infrequent and very frequent terms
#  - IMPORTANT: name the resulting object "dtm"

# aggregate articles at year level ----

# NOTE: the data is too granular (article-level) to get meaningful Wordfish estimates
hist(df$metadata__date, breaks = 20)

# so let's aggregate by year
docvars(dtm, "year") <- as.integer(format(docvars(dtm, "metadata__date"), format = "%Y"))

dtm_years <- dfm_group(dtm, group = docvars(dtm, "year"))

# fit the wordfish model ----

# TODO: fit the wordfish model to the yearly-aggregated DTM
#  - IMPORTANT: name the resulting object "wf_years"
# NOTE: this may take a while to compute
# NOTE: if you get an out-of-memory or RAM-related error, reduce the corpus' size
#        for example by subset to articles ≥ 1980 or sampling articles within years

# let's get the position estimates and plot them over time

thetas <- as.data.frame(coef(wf_years, "documents"))
thetas["theta_se"] <- wf_years$se.theta
thetas["year"] <- as.integer(rownames(thetas))

ggplot(
  data = thetas, 
  mapping = aes(x = year, y = theta, ymin = theta - 1.96 * theta_se, ymax = theta + 1.96 * theta_se)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2)
# TODO: interpret the results

# compare to human-coded sentiment ----

df["year"] <- as.integer(format(df$metadata__date, format = "%Y"))

# let's plot yearly averages of articles' sentiment using human-coders average article sentiment ratings
df |> 
  group_by(year) |> 
  summarise(
    sent = mean(sentiment),
    sent_sd = sd(sentiment)
  ) |> 
  ggplot(
    mapping = aes(x = year, y = sent, ymin = sent - 1.96 * sent_sd, ymax = sent + 1.96 * sent_sd)
  ) +
  geom_line() +
  geom_ribbon(alpha = 0.2)

# TODO: compare the plot of human coding-based sentiment scores to that of Wordfish estimates
#        what can we learn from it about the relation between the two measures?

# TODO: correlate human coding-based sentiment scores and Word-fish estimates
#        what can we learn from it about the relation between the two measures?
