# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Induce populist dictionary terms from human-coded documents
#' @author Hauke Licht
#' @date   2025-10-27
#' @note   We use data from the paper 
#' 
#'           Dai, Y., & Kustov, A. (2022). When Do Politicians Use Populist 
#'               Rhetoric? Populism as a Campaign Gamble. 
#'               _Political Communication_, 39(3), 383–404. 
#'               https://doi.org/10.1080/10584609.2022.2025505
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
# TODO: install the "lexicon" package if you don't have it yet
library(readr)
library(dplyr)
library(ggplot2)
set_theme(theme_minimal())

source(file.path("R", "fightin_words.R"))

# load and prepare the data ----

fp <- file.path("data", "labeled", "dai_when_2022", "dai_when_2022-campaignspeech_populism.csv")

df <- read_csv(fp)

# NOTE: the label categories are "yes" (populist) and "no" (not populist)
df$label <- factor(df$label, c("yes", "no"), c("populist", "not populist"))

# use pre-define train/test split
table(df$metadata__split)

df_train <- df[df$metadata__split == "train", ]

# TODO: construct the DFM for the train split ----
corp <- corpus(df_train, text_field = "text", docid_field = "uid")

dtm_trimmed <- corp |> 
  tokens(
    remove_punct=TRUE, 
    remove_symbols=TRUE,
    remove_numbers=TRUE
  ) |> 
  as.tokens() |>
  tokens_tolower() |> 
  # tokens_remove(stopwords("en")) |> 
  # remove chunk words
  tokens_remove(c("xa0")) |> 
  # NOTE: we add something new here
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |>  
  dfm() |> 
  dfm_trim(
    min_termfreq = 10, termfreq_type = "count",
    max_docfreq = 0.85, docfreq_type = "prop",
  )

# TODO: group DFM by label
dtm_by_label <- dfm_group(x = dtm_trimmed, groups = docvars(dtm_trimmed, "label"))

# TODO: compute fightin' words statistics
fw_stats <- textstat_fighting_words(
  x = dtm_by_label, 
  group.var = "label", 
)
# subset to relevant contrast
fw_stats <- fw_stats[[1]] 

# TODO: construct dictionary of significant populist and non-populist terms
significant_zscores <- fw_stats |> 
  # for each party group
  group_by(label) |>
  # sort from highest to lowest
  arrange(desc(abs(z_score))) |> 
  ungroup() |> 
  filter(abs(z_score) >= 1.96)

# visualize
plot_scores(
  significant_zscores,
  value.var = "z_score",
  group.var = "label",
  xlab = "Fightin' words z-score",
  ncol = 1
) + 
  geom_vline(xintercept = 1.96, linetype = "dashed", color = "gray70") + 
  geom_vline(xintercept = -1.96, linetype = "dashed", color = "gray70")

# NOTE: the problem is that there are very few significant non-populist terms

populism_dict <- fw_stats |> 
  filter(label == "populist") |> 
  with(split(feature, as.character(label))) |> 
  dictionary()


# apply the dictionary ----

## TODO: apply to train split ----

docvars_train <- docvars(dtm_trimmed)
docvars_train$uid <- docnames(dtm_trimmed)
docvars_train$n_toks <- ntoken(dtm_trimmed)

df_train_scored <- dtm_trimmed |> 
  dfm_lookup(dictionary = populism_dict) |> 
  convert(to = "data.frame", docid_field = "uid") |> 
  left_join(docvars_train, by = "uid") |> 
  # NOTE: we apply the same transforamtion as Proksch et al. (2019) use for sentiment
  mutate(score = populist/n_toks) 

# conduct t-test
t.test(score ~ label, data = df_train_scored)

# let's see if documents that human coders have assigned to the "economy" topic
#  also have higher dictionary scores
ggplot(df_train_scored, aes(x = label, y = score, fill = label)) + 
  geom_boxplot(alpha = 0.8, color = "darkgrey") + 
  labs(
    x = "topic",
    y = "'economicness' dictionary scores (log odds ratio)"
  ) + 
  theme(legend.position = "bottom")


## TODO: apply to test split ----

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
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  dfm()

# NOTE: no trimming needed because we are applying an existing dictionary

docvars_test <- docvars(dfm_test)
docvars_test$uid <- docnames(dfm_test)
docvars_test$n_toks <- ntoken(dfm_test)

df_test_scored <- dfm_test |> 
  dfm_lookup(dictionary = populism_dict) |> 
  convert(to = "data.frame", docid_field = "uid") |> 
  left_join(docvars_test, by = "uid") |>
  mutate(score = populist/n_toks)

# let's combine the scores fro train and test split documents and visualize
bind_rows(
  "in-sample" = df_train_scored,
  "out-of-sample" = df_test_scored,
  .id = "eval"
) |> 
  ggplot(aes(x = eval, y = score, fill = label)) + 
  geom_boxplot(alpha = 0.8, color = "darkgrey") + 
  labs(
    x = NULL,
    y = "populist score (log odds ratio)"
  ) + 
  theme(legend.position = "bottom")

# NOTE: This illustrates two key problems of dictionary construction:
#       1. The terms that seem discriminative in the data you look at
#           when constructing the dictionary (here: train split) may be
#           less discriminative in new data (here: test split)
#          This means that dictionaries often "overfit" to the data they
#           were constructed on and do not "generalize" well to new data.
#       2. Very few of the selected keywords may actually occur in new data.
#          This can be seen in the figure in the low proportions of populist
#          scores in the test split.

