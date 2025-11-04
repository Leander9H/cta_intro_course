# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Validate Sentiment Dictionaries against human raters sentiment scores 
#' @author Hauke Licht
#' @date   2025-10-2
#' @note   We use data from the paper <https://doi.org/10.1017/pan.2020.8>
#' 
#'           Barberá P, Boydstun AE, Linn S, McMahon R, Nagler J. "Automated Text 
#'              Classification of News Articles: A Practical Guide." 
#'              _Political Analysis_. 2021;29(1):19-42. doi:10.1017/pan.2020.8
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
# TODO: renv::install("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
library(readr)
library(dplyr)
library(ggplot2)
set_theme(theme_minimal())

# load and prepare the data ----

fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_sentiment.csv")

df <- read_csv(fp)

glimpse(df)
# NOTE: column "label" records average of multiple coders' sentiment ratings

df <- rename(df, sentiment = label)

# check that no NAs
table(is.na(df$sentiment))

# show empirical range
range(df$sentiment)
# NOTE: the theoretical range is 0-10 (scale used by Barbera et al's annotators)

# show distribution
ggplot(df, aes(sentiment)) +
  geom_histogram() +
  scale_x_continuous(breaks = 0:10, limits = c(0,10))

# create a DTM ----

corp <- corpus(df, text_field = "text", docid_field = "uid")

dtm <- corp |> 
  tokens(
    remove_punct=TRUE, 
    remove_symbols=TRUE,
    remove_numbers=TRUE
  ) |> 
  as.tokens() |>
  tokens_tolower() |> 
  tokens_remove(stopwords("en")) |>
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  dfm()

# NOTE: no need to trim the DFM for dictionary application

# apply LSD dictionary ----

data(package = "quanteda.sentiment")
data("data_dictionary_AFINN", package = "quanteda.sentiment")

data_dictionary_AFINN

# read the dictioanr documentation
help(data_dictionary_LSD2015, package = "quanteda.sentiment")
glimpse(docvars(dtm))

df_scored <- dtm |> 
  dfm_lookup(dictionary = data_dictionary_LSD2015) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dtm)) |> 
  mutate(
    total_neg = negative+neg_positive,
    total_pos = positive+neg_negative,
    # see formula (1) in Proksch et al. (2019) and Lowe et al. (2011)
    score = log( (total_pos+0.5) / (total_neg+0.5) )
  )
  
View(df_scored)
corr <- cor(df_scored$sentiment, df_scored$score, method = "pearson")

ggplot(df_scored, aes(x = sentiment, y = score)) + 
  geom_point(alpha = 0.8, size = 0.1, color = "darkgray") +
  geom_smooth(method = "lm", color = "black") +
  lims(
    x = c(0, 10),
    y = range(df_scored$score)*c(0.9, 1.1)
  ) +
  labs(
    title = paste0("Pearson's r = ", round(corr, 2)),
    x = "average human rating (0-10)",
    y = "LSD2015 dictionary scores (log odds ratio)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") 

# NOTE: the correlation with human's sentiment judgments is only very weak

# with other dictionaries ----

# TODO: replicate the above analysis with other dictionaries below

# see available dictionaries
data(package = "quanteda.sentiment")

## Hu & Liu ----

data("data_dictionary_HuLiu", package = "quanteda.sentiment")

# NOTE: Hu & Liu dictionary has only positive and negative categories
names(data_dictionary_HuLiu)

df_scored <- dtm |> 
  dfm_lookup(dictionary = data_dictionary_HuLiu) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dtm)) |> 
  mutate(
    total_pos = positive,
    total_neg = negative,
    # see formula (1) in Proksch et al. (2019) and Lowe et al. (2011)
    score = log( (total_pos+0.5) / (total_neg+0.5) )
  )

corr <- cor(df_scored$sentiment, df_scored$score, method = "pearson")

# boxplot of scores by sentiment
ggplot(df_scored, aes(x = sentiment, y = score)) + 
  geom_point(alpha = 0.8, size = 0.1, color = "darkgray") +
  geom_smooth(method = "lm", color = "black") +
  lims(
    x = c(0, 10),
    y = range(df_scored$score)*c(0.9, 1.1)
  ) +
  labs(
    title = paste0("Pearson's r = ", round(corr, 2)),
    x = "average human rating (0-10)",
    y = "Hu & Liu dictionary scores (log odds ratio)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") 


## Augmented General Inquirer Positiv and Negativ dictionary ----

help(data_dictionary_geninqposneg, package = "quanteda.sentiment")

data("data_dictionary_geninqposneg", package = "quanteda.sentiment")

df_scored <- dtm |> 
  dfm_lookup(dictionary = data_dictionary_geninqposneg) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dtm)) |> 
  mutate(
    # see formula (1) in Proksch et al. (2019) and Lowe et al. (2011)
    score = log( (positive+0.5) / (negative+0.5) )
  )

corr <- cor(df_scored$sentiment, df_scored$score, method = "pearson")

# boxplot of scores by sentiment
ggplot(df_scored, aes(x = sentiment, y = score)) + 
  geom_point(alpha = 0.8, size = 0.1, color = "darkgray") +
  geom_smooth(method = "lm", color = "black") +
  lims(
    x = c(0, 10),
    y = range(df_scored$score)*c(0.9, 1.1)
  ) +
  labs(
    title = paste0("Pearson's r = ", round(corr, 2)),
    x = "average human rating (0-10)",
    y = "Augmented General Inquirer Positiv/Negativ scores (log odds ratio)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") 

## Loughran & McDonald ----

data("data_dictionary_LoughranMcDonald", package = "quanteda.sentiment")

# NOTE: Loughran & McDonald dictionary has additional categories
data_dictionary_LoughranMcDonald


df_scored <- dtm |> 
  dfm_lookup(dictionary = data_dictionary_LoughranMcDonald[c("POSITIVE", "NEGATIVE")]) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dtm)) |> 
  mutate(
    # see formula (1) in Proksch et al. (2019) and Lowe et al. (2011)
    score = log( (POSITIVE+0.5) / (NEGATIVE+0.5) )
  )

corr <- cor(df_scored$sentiment, df_scored$score, method = "pearson")

# boxplot of scores by sentiment
ggplot(df_scored, aes(x = sentiment, y = score)) + 
  geom_point(alpha = 0.8, size = 0.1, color = "darkgray") +
  geom_smooth(method = "lm", color = "black") +
  lims(
    x = c(0, 10),
    y = range(df_scored$score)*c(0.9, 1.1)
  ) +
  labs(
    title = paste0("Pearson's r = ", round(corr, 2)),
    x = "average human rating (0-10)",
    y = "Hu & Liu dictionary scores (log odds ratio)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") 


## VADER ----

data("data_dictionary_vader", package = "quanteda.sentiment")

# NOTE: VADER dictionary has only positive terms
#       so we need to compute the score differently (see below)
names(data_dictionary_vader)

# NOTE: 
df_scored <- dtm |> 
  dfm_lookup(dictionary = data_dictionary_vader, valuetype = "fixed") |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dtm)) |> 
  mutate(
    # compute share of positive terms relative to all terms in a document
    score = VADER/nfeat(dtm)
  )

corr <- cor(df_scored$sentiment, df_scored$score, method = "pearson")
print(corr)

# boxplot of scores by sentiment
ggplot(df_scored, aes(x = sentiment, y = score)) + 
  geom_point(alpha = 0.8, size = 0.1, color = "darkgray") +
  geom_smooth(method = "lm", color = "black") +
  lims(
    x = c(0, 10),
    y = range(df_scored$score)*c(0.9, 1.1)
  ) +
  labs(
    title = paste0("Pearson's r = ", round(corr, 2)),
    x = "average human rating (0-10)",
    y = "Hu & Liu dictionary scores (log odds ratio)"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") 

