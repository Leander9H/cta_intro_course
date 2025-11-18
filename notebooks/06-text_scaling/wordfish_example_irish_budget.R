# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Example of fitting a Wordfish model to Irish budget speeches
#' @note   based on https://tutorials.quanteda.io/machine-learning/wordfish/
#' @author Hauke Licht
#' @date   2025-11-18
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

require(quanteda)
require(quanteda.textmodels) # TODO (if needed) `renv::install("quanteda.textmodels")`
require(quanteda.textplots) # TODO (if needed) `renv::install("quanteda.textplots")`

# prepare the data ----

toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)
sparsity(dfmat_irish)


# fit the wordfish model ----

tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(6, 5))
# NOTE: the `dir = c(6, 5)` argument sets the direction of the latent dimension
#  such that the score of document 6 (see `docnames(dfmat_irish)[6]`) is lower
#  than that of document 5 (see `docnames(dfmat_irish)[5]`)
# This is an "identification constraint" necessary for the model to be estimable.
#  by default, `textmodel_wordfish()` would set document 1 < document 2, which 
#  may or may not be appropriate depending on the data and contents of these 
#  documents.

# inspect the estimates ----
summary(tmod_wf)

# let's plot documents' position estimates
textplot_scale1d(tmod_wf, "documents")

# NOTE: here is how to directly extract the document position estimates and standard errors
thetas <- as.data.frame(coef(tmod_wf, "documents"))
thetas["theta_se"] <- tmod_wf$se.theta
thetas["doc_id"] <- rownames(thetas)

doc_vars <- docvars(dfmat_irish)
doc_vars["doc_id"] <- docid(dfmat_irish)

thetas_df <- merge(doc_vars, thetas, by = "doc_id")

# with this info you can create all sorts of plots or summaries, e.g.
library(dplyr)
thetas_df |> 
  group_by(party) |>
  summarise(
    mean_theta = mean(theta),
    sd_theta = sd(theta),
    n = n()
  ) |> 
  arrange(desc(mean_theta))


