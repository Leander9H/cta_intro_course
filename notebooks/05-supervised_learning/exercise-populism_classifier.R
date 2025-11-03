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

# NOTE: the label categories are "yes" (populist) and "no" (not populist)
df$populist <- factor(df$label, c("accept", "reject"), c("yes", "no"))

# use pre-define train/test split
table(df$metadata__split)

prop.table(with(df, table(metadata__split, populist)), 1)

# TODO: construct the DFM

# TODO: create the train data (using docvar "metadata__split") and apply further preprocessing if needed

# TODO: fit the NB model

# TODO what are the most important features for the populist label class?

# TODO: evaluate predictive performance in terms of precision, recall and the F1 score

# TODO: evaluate the models predictive performance for candidates "Donald J. Trump" and "Barack Obama"





