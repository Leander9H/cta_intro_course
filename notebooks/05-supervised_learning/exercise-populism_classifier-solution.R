# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Train a Naive Bayes populism classifier
#' @author Hauke Licht
#' @date   2025-11-04
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
library(quanteda.textmodels)
library(readr)
library(dplyr)
library(ggplot2)
set_theme(theme_minimal())

# load and prepare the data ----

fp <- file.path("data", "labeled", "dai_when_2022", "dai_when_2022-campaignspeech_populism.csv")

df <- read_csv(fp)

# NOTE: the label categories are "accept" (populist) and "reject" (not populist)
df$populist <- factor(df$label, c("accept", "reject"), c("yes", "no"))

# use pre-define train/test split
table(df$metadata__split)

prop.table(with(df, table(metadata__split, populist)), 1)


# TODO: construct the DFM ----

df_train <- df[df$metadata__split == "train", ]

dtm <- df |> 
  corpus(text_field = "text", docid_field = "uid") |> 
  tokens(
    remove_punct=TRUE, 
    remove_symbols=TRUE,
    remove_numbers=TRUE
  ) |> 
  tokens_tolower() |> 
  tokens_remove(stopwords("en")) |> 
  # remove chunk words
  tokens_remove(c("xa0")) |> 
  # NOTE: we add something new here
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |>  
  dfm()

# TODO: create the train data and apply trimming 
dtm_train <- dfm_subset(dtm, docvars(dtm, "metadata__split") == "train")
dtm_train <- dfm_trim(
  dtm_train,
  min_termfreq = 10, termfreq_type = "count",
  max_docfreq = 0.75, docfreq_type = "prop",
)

# TODO: fit the NB model
nb_model <- textmodel_nb(
  x = dtm_train,
  y = docvars(dtm_train, "populist"),
  prior = "docfreq",
  distribution = "Bernoulli"
)

# TODO: what are the 20 features that contribute most to classification decisions?
names(sort(rowSums(coef(nb_model)), decreasing = TRUE)[1:20])

# TODO what are the most important features for the populist label class?
nb_model |>
  coef() |> 
  as.data.frame() |> 
  mutate(log_or = log(yes / no)) |> 
  arrange(desc(log_or)) |> 
  head(10)

# TODO: evaluate predictive performance
dtm_test <- dfm_subset(dtm, docvars(dtm, "metadata__split") == "test")
y_true <- docvars(dtm_test, "populist")
y_pred <- predict(nb_model, newdata = dtm_test, force = TRUE)

cm <- table(Actual = y_true, Predicted = y_pred)
cm

tp <- cm["yes", "yes"]
tn <- cm["no",  "no" ]
fp <- cm["no",  "yes"]
fn <- cm["yes", "no" ]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)
precision; recall; f1_score

# TODO: evaluate the models predictive performance for candidates "Donald J. Trump" and "Barack Obama"

table(docvars(dtm_test, "metadata__candidate"))

dtm_test_trump <- dfm_subset(dtm_test, docvars(dtm_test, "metadata__candidate") == "Donald J. Trump")
dtm_test_obama <- dfm_subset(dtm_test, docvars(dtm_test, "metadata__candidate") == "Barack Obama")

# how many examples are there per candidate?
table(docvars(dtm_test_trump, "populist"))
table(docvars(dtm_test_obama, "populist"))

# define helper functions for easy evaluation
compute_metrics <- function(true, pred) {
  cm <- table(true, pred)
  
  tp <- cm["yes", "yes"]
  tn <- cm["no",  "no" ]
  fp <- cm["no",  "yes"]
  fn <- cm["yes", "no" ]
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(c("precision" = precision, "recall" = recall, "F1" = f1_score))
}

evaluate <- function(x, model, label.col) {
  y_true <- docvars(x, label.col)
  y_pred <- suppressWarnings(predict(model, newdata = x, force = TRUE))
  return(compute_metrics(y_true, y_pred))
}

evaluate(x = dtm_test_trump, model = nb_model, label.col = "populist")
evaluate(x = dtm_test_obama, model = nb_model, label.col = "populist")
# NOTE: the classifier struggles more with correctly classifying Trump's speeches




