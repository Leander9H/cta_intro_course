# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Compare Wordfish estimates to human-coded sentiment estimates
#' @author Hauke Licht
#' @date   2025-11-17
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(quanteda.corpora)
library(quanteda.textmodels) # TODO: (if needed) `renv::install("quanteda.textmodels")`
library(quanteda.textplots) # TODO: (if needed) `renv::install("quanteda.textplots")`
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
# NOTE: the data is too granular (article-level) to get meaningful Wordfish estimates


# prepare the document-term matrix ----

dtm <- df |> 
  corpus(text_field = "text", docid_field = "uid") |> 
  tokens(remove_punct = TRUE, remove_symbols = FALSE, remove_numbers = FALSE) |> 
  tokens_tolower() |> 
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  tokens_remove(stopwords("en")) |>
  dfm() |> 
  dfm_trim(
    min_termfreq = 10, termfreq_type = "count",
    max_docfreq = 0.85, docfreq_type = "prop"
  )

# aggregate articles at year level ----

# so let's aggregate by year
hist(df$metadata__date, breaks = 20)

docvars(dtm, "year") <- as.integer(format(docvars(dtm, "metadata__date"), format = "%Y"))

dtm_years <- dfm_group(dtm, group = docvars(dtm, "year"))
dociddtm_years

# fit the wordfish model ----
wf_years <- textmodel_wordfish(dtm_years)
# NOTE: this may take a while to compute
# NOTE: if you get an out-of-memory or RAM-related error, reduce the corpus' size
#        for example by subset to articles ≥ 1980 or sampling articles within years

# inspect he estimates
textplot_scale1d(wf_years, "documents")

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

# NOTE: the wordfish estimates suggest that sentiment has been declining over time

# BUT look at the most discriminative words ...
betas <- as.data.frame(coef(wf_years, "features"))
betas["token"] <- rownames(betas)
betas |> 
  as.data.frame() |> 
  group_by(sing = sign(beta)) |> 
  slice_max(order_by = abs(beta), n = 5)
# ... these aren't clear sentiment words
# ... and actual sentiment words don't contribute much to scale locations
betas["good", ]
betas["bad", ]
betas["poor", ]

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

# NOTE: the human coding-based evidence contradicts the Wordfish estimates

# let's correlate human coding-based sentiment scores and Word-fish estimates

yearly_measures_df <- df |> 
  group_by(year) |> 
  summarise(sentiment = mean(sentiment)) |> 
  left_join(thetas, by = "year")

with(yearly_measures_df, cor.test(sentiment, theta))
# NOTE: near zero, stat. insignificant correlation, indicating that the 
#  Wordfish estimates measure sth different than sentiment

