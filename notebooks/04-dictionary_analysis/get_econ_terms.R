# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Induce dictionary terms for human-coded documents
#' @author Hauke Licht
#' @date   2025-10-27
#' @note   We use data from the paper <https://doi.org/10.1017/pan.2020.8>
#' 
#'           Barberá P, Boydstun AE, Linn S, McMahon R, Nagler J. "Automated Text 
#'              Classification of News Articles: A Practical Guide." 
#'              _Political Analysis_. 2021;29(1):19-42. doi:10.1017/pan.2020.8
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(readr)
library(dplyr)
library(ggplot2)
set_theme(theme_minimal())

source(file.path("R", "fightin_words.R"))

# load and prepare the data ----

fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_topic.csv")

df <- read_csv(fp)

# NOTE: the label categories are "yes" (about economy) and "no" (not about economy)
df$label <- factor(df$label, c("yes", "no"), c("economy", "other"))

# split into train and test set
#
#' NOTE: we split into two sets here
#' 
#'  - train set: used to induce the dictionary terms using the fightin' words approach
#'  - test set: used to evaluate the induced dictionary terms
#'  
#' This will make more sense when we talk about supervised learning next week ;)
set.seed(1234)
df$metadata__split <- sample(
  x = c("train", "test"), 
  size = nrow(df), 
  replace = TRUE, 
  prob = c(0.8, 0.2)
)

# induce dictionary terms with fightin' words ----

## constuct DFM ----

df_train <- df[df$metadata__split == "train", ]

corp <- corpus(df_train, text_field = "text", docid_field = "uid")

dfm_trimmed <- corp |> 
  tokens(
    remove_punct=TRUE, 
    remove_symbols=TRUE,
    remove_numbers=TRUE
  ) |> 
  as.tokens_xptr() |>
  tokens_tolower() |> 
  tokens_remove(stopwords("en")) |>
  # NOTE: we add something new here: n-grams (see https://en.wikipedia.org/wiki/N-gram)
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  dfm() |> 
  dfm_trim(
    min_termfreq = 10, termfreq_type = "count",
    max_docfreq = 0.85, docfreq_type = "prop"
  )

# group by label category (required by fightin' words method for group's word usage differences)
dtm_by_label <- dfm_group(x = dfm_trimmed, groups = docvars(dfm_trimmed, "label"))

## apply fightin' words ----

fw_stats <- textstat_fighting_words(
  x = dtm_by_label, 
  group.var = "label", 
)
# there is only one contrast (because only two categories): "economy" vs "other"
names(fw_stats)
# subset to estimates for this contrast
fw_stats <- fw_stats[[1]] 

# look at example output
fw_stats |> 
  select(feature, z_score, label) |> 
  head(6)

## visualize z-scores ----
fw_stats |> 
  # for each party group
  group_by(label) |>
  # sort from highest to lowest
  arrange(desc(abs(z_score))) |> 
  # get top terms
  slice_head(n = 60) |> 
  ungroup() |> 
  # visualize (note `plot_scores` comes from R/fightin_words.R script)
  plot_scores(
    value.var = "z_score",
    group.var = "label",
    xlab = "Fightin' words z-score",
  ) + 
    geom_vline(xintercept = 1.96, linetype = "dashed", color = "gray70") + 
    geom_vline(xintercept = -1.96, linetype = "dashed", color = "gray70")

## put terms with significant z-scores in dictionary ----

topic_dict <- fw_stats |> 
  filter(abs(z_score) >= 1.96) |> 
  with(split(feature, label)) |> 
  dictionary()

topic_dict

# apply the dictionary ----

### apply to train split ----

# NOTE: we first apply the dictionary to the same documents we used to induce it
#       This is for illustration purposes only because usually, we would apply it
#       to unseen documents (i.e. the test split below) because why construct a 
#       dictionary if you can just use the labels you already have, right?

df_train_scored <- dfm_trimmed |> 
  dfm_lookup(dictionary = topic_dict) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dfm_trimmed)) |> 
  as_tibble() |> 
  # NOTE: we apply the same transforamtion as Proksch et al. (2019) use for sentiment
  mutate(score = log((economy+0.5) / (other+0.5))) 

# let's see if documents that human coders have assigned to the "economy" topic
#  also have higher dictionary scores
ggplot(df_train_scored, aes(x = label, y = score, fill = label)) + 
  geom_boxplot(alpha = 0.8, color = "darkgrey") + 
  labs(
    x = "topic",
    y = "'economicness' dictionary scores (log odds ratio)"
  ) + 
  lims(y = c(-7, 7)) + 
  theme(legend.position = "bottom")
  
## apply to test split ----

# Now to the more realistic scenario: applying the dictionary to "unseen" documents

df_test <- df[df$metadata__split == "test", ]

corp_test <- corpus(df_test, text_field = "text", docid_field = "uid")

dfm_test <- corp_test |> 
  tokens(
    remove_punct=TRUE, 
    remove_symbols=TRUE,
    remove_numbers=TRUE
  ) |> 
  as.tokens_xptr() |>
  tokens_tolower() |> 
  # NOTE: we add something new here
  tokens_ngrams(n = 1:3) |> 
  # tokens_wordstem(language = "en") |> 
  # ~stem~ lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  dfm()
  # NOTE: no trimming needed because we are applying an existing dictionary

df_test_scored <- dfm_test |> 
  dfm_lookup(dictionary = topic_dict) |> 
  convert(to = "data.frame", docid_field = "articleid") |> 
  bind_cols(docvars(dfm_test)) |> 
  as_tibble() |> 
  mutate(score = log((economy+0.5) / (other+0.5)))

# let's combine the scores fro train and test split documents and visualize
bind_rows(
  "in-sample" = df_train_scored,
  "out-of-sample" = df_test_scored,
  .id = "eval"
) |> 
  ggplot(aes(x = eval, y = score, fill = label)) + 
    geom_boxplot(alpha = 0.8, color = "darkgrey") + 
    lims(y = c(-7, 7)) + 
    labs(
      x = NULL,
      y = "'economicness' dictionary scores (log odds ratio)"
    ) + 
    theme(legend.position = "bottom")

# NOTE: This illustrates a key problem of dictionary construction:
#        the terms that seem discriminative in the data you look at
#        when constructing the dictionary (here: train split) may be
#        less discriminative in new data (here: test split)
#       This means that dictionaries often "overfit" to the data they
#        were constructed on and do not "generalize" well to new data.
#       See, e.g., https://en.wikipedia.org/wiki/Overfitting
