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

# TODO: construct the DFM for the train split

# TODO: group DFM by label

# TODO: compute fightin' words statistics

# TODO: construct dictionary of significant populist and non-populist terms

# TODO: apply to train split

# TODO: apply to test split
