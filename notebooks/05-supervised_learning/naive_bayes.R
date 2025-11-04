# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from notebook `naive_bayes.html`
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2025-11-04
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #


## ----setup--------------------------------------------------------------


## ----libraries----------------------------------------------------------
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------------------
library(ggplot2)
set_theme(
  theme_minimal() + 
    theme(
      theme(
        panel.background = element_rect(fill = "transparent", color = NA_character_),
        # panel.grid.major = element_blank(), # get rid of major grid
        # panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent", color = NA_character_),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")
    )
  )
)


## ----data---------------------------------------------------------------
# NOTE: assuming you run this file in th context of our courses R project
fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_topic.csv")

# load the CSV file
df <- read_csv(fp)

# NOTE: the label categories are "yes" (about economy) and "no" (not about economy)
df$label <- factor(df$label, c("yes", "no"), c("economy", "other"))


## ----create_splits_metadata---------------------------------------------
# NOTE: set random number generation seed for reproducibility
set.seed(1234) 

# randomly assign 20% of documents to test split
df$metadata__split <- sample(
  size = nrow(df), 
  x = c("train", "test"), 
  prob = c(0.8, 0.2),
  replace = TRUE 
)


## -----------------------------------------------------------------------
df |> 
  with(table( metadata__split, label)) |> 
  prop.table(1) |>
  round(3)


## ----dtm----------------------------------------------------------------
dtm <- df |> 
  corpus(text_field = "text", docid_field = "uid") |> 
  tokens(remove_symbols=FALSE, remove_numbers=FALSE) |> 
  tokens_tolower() |> 
  tokens_ngrams(n = 1:3) |> 
  # lemmatize (based on  https://stackoverflow.com/a/62330539)
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) |> 
  tokens_remove(stopwords("en")) |>
  dfm()


## ----create_splits------------------------------------------------------
# get the train split data and trim it
dtm_train <- dtm |>
  dfm_subset(docvars(dtm, "metadata__split") == "train") |>
  dfm_trim(
    min_termfreq = 10, termfreq_type = "count",
    max_docfreq = 0.85, docfreq_type = "prop"
  )
sparsity(dtm_train)
dim(dtm_train)


## -----------------------------------------------------------------------
# get the test split data and only keep features that are also in the train set
dtm_test <- dfm_subset(dtm, docvars(dtm, "metadata__split") == "test")


## ----model--------------------------------------------------------------
nb_model <- textmodel_nb(
  x = dtm_train, # the document feature matrix with trainining documents text representations
  y = docvars(dtm_train, "label"), # the vector indicating training documents' true labels
  distribution = "Bernoulli" # set this to binarize feature counts (otherwise "multinomial")
)


## -----------------------------------------------------------------------
dim(nb_model$param)
nfeat(dtm_train)


## -----------------------------------------------------------------------
# let's look at parameters for first five features
coef(nb_model)[1:5, ]


## -----------------------------------------------------------------------
feat_imp <- as.data.frame(coef(nb_model))
feat_imp$log_odds <- log( feat_imp$economy / feat_imp$other )


## -----------------------------------------------------------------------
feat_imp |> 
  arrange(desc(log_odds)) |> 
  head(10)


## -----------------------------------------------------------------------
feat_imp |> 
  arrange(log_odds) |> 
  head(10)


## -----------------------------------------------------------------------
feat_imp["sale", ]


## ----pred class---------------------------------------------------------
y_test <- predict(nb_model, newdata = dtm_test, force = TRUE)


## -----------------------------------------------------------------------
# get the predicted labels of the first 5 documents
y_test[1:5]


## -----------------------------------------------------------------------
table(y_test)


## ----pred prob----------------------------------------------------------
y_test_probs <- predict(nb_model, newdata = dtm_test, type = "probability", force = TRUE)


## -----------------------------------------------------------------------
# get the predicted class probablities of the first 5 documents
y_test_probs[1:5, ]


## -----------------------------------------------------------------------
ggplot(mapping = aes(x = y_test_probs[ , 1])) + 
  geom_histogram() + 
  xlab('pred. prob. for label class "economy"')


## ----evaluation---------------------------------------------------------
y_pred <- predict(nb_model, newdata = dtm_test, force = TRUE)


## ----confusion_matrix---------------------------------------------------
y_true <- docvars(dtm_test, "label")
cm <- table(Predicted = y_pred, Actual = y_true)
cm


## -----------------------------------------------------------------------
tp <- cm["economy", "economy"]
tn <- cm["other", "other"]
fp <- cm["economy", "other"]
fn <- cm["other", "economy"]

cat(
  "True Positives  (economy ✅): ", tp, "\n",
  "True Negatives  (other ✅):   ", tn, "\n",
  "False Positives (economy ❌): ", fp, "\n",
  "False Negatives (other ❌):   ", fn, "\n",
  sep = ""
)


## ----accuracy-----------------------------------------------------------
accuracy <- (tp + tn) / sum(cm)
cat("Accuracy: ", round(accuracy, 3))


## ----precision----------------------------------------------------------
cat("No. of predicted 'economy' cases: ", tp + fp, "\n")
cat("No. of these that are correct:    ", tp, "\n")

precision <- tp / (tp + fp)
cat("↳ Precision: ", round(precision, 3))


## ----recall-------------------------------------------------------------
cat("No. of actual 'economy' cases: ", tp + fn, "\n")
cat("No. of these that are correct:  ", tp, "\n")

recall <- tp / (tp + fn)
cat("↳ Recall: ", round(recall, 3))


## ----f1_score-----------------------------------------------------------
f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score


## -----------------------------------------------------------------------
# add year and decade indicators to doc vars
dtm_test@docvars <- dtm_test@docvars |> 
  mutate(
    metadata__year = as.integer(format(metadata__date, "%Y")),
    metadata__decade = round(metadata__year, -1)
  )

table(docvars(dtm_test, "metadata__decade"))


## -----------------------------------------------------------------------
compute_precision <- function(true, pred) {
  cm <- table(pred, true)
  tp <- cm[1, 1]
  fp <- cm[1, 2]
  return( tp / (tp + fp) )
}

compute_recall <- function(true, pred) {
  cm <- table(pred, true)
  tp <- cm[1, 1]
  fn <- cm[2, 1]
  return( tp / (tp + fn) )
}

decades <- sort(unique(docvars(dtm_test, "metadata__decade")))

mets <- matrix(NA_real_, nrow = length(decades), ncol = 2)
rownames(mets) <- decades
colnames(mets) <- c("recall", "precision")

for (dec in decades) {
  sub_dtm = dfm_subset(dtm_test, docvars(dtm_test, "metadata__decade") == dec)
  y_true <- docvars(sub_dtm, "label")
  y_pred <- suppressWarnings(predict(nb_model, sub_dtm, force = TRUE))
  mets[as.character(dec), 1] <- compute_recall(y_true, y_pred)
  mets[as.character(dec), 2] <- compute_precision(y_true, y_pred)
}

mets <- as.data.frame(mets)

mets$pr_ratio <- mets$precision / mets$recall

mets$decade <- row.names(mets)

mets


## -----------------------------------------------------------------------
mets |>
  ggplot(mapping = aes(y = pr_ratio, x = decade)) + 
    geom_col(alpha = 0.9) + 
    ylim(0, 2) +
    labs(y = "ratio: precision / recall", x = "decade")

