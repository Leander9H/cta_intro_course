# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from slides `03-word_use_differences.pdf`
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2025-10-21
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(quanteda)
library(dplyr)
library(tidyr)
library(ggplot2)
set_theme(theme_minimal())

# load and prepare the data ----
data(
	"data_corpus_ukmanifestos", 
	package = "quanteda.corpora"
)
toks <- tokens(
	data_corpus_ukmanifestos, 
	remove_punct=TRUE, 
	remove_symbols=TRUE,
	remove_numbers=TRUE
)
toks <- tokens_remove(toks, stopwords("en"))
dtm <- dfm(toks, tolower=TRUE)

head(docvars(dtm)[1:5], 3)

# subset to selected parties' manifestos
grp_var <- "Party"
parties <- c("Lab", "Con")
dtm <- dfm_subset(
  x = dtm, 
  subset = docvars(dtm, grp_var) %in% parties
)
# aggregate docs at group level
grouped_corp <- dfm_group(
	x = dtm, 
	groups = docvars(dtm, grp_var)
)
ndoc(grouped_corp)
docnames(grouped_corp)


# ensure row order: Labour, Conservative
grouped_corp <- grouped_corp[match(parties, docnames(grouped_corp)), ]  


# define helper functions ----
get_top_n_terms <- function(x, groups = c("Lab", "Con"), .n = 20) {
  a <- head(sort(x, decreasing = TRUE), n = .n)
  a <- tibble::enframe(a, name = "feature")
  a["group"] <- groups[[1]]
  
  b <- head(sort(x, decreasing = FALSE), n = .n)
  b <- tibble::enframe(b, name = "feature")
  b["group"] <- groups[[2]]
  
  dplyr::bind_rows(a, b)
}

plot_scores <- function(x, feature.var = "feature", value.var = "value", group.var = "group", xlab = "Score") {
  x_lims_ <- max(abs(x[[value.var]]), na.rm = TRUE) * c(-1.05, 1.05)
  ggplot(
    data = x,
    mapping = aes(
      y    = reorder(.data[[feature.var]], abs(.data[[value.var]])),
      x    = .data[[value.var]],
      fill = .data[[group.var]]
    )
  ) +
    geom_col() + xlim(x_lims_) +
    facet_wrap(vars(.data[[group.var]]), ncol = 2, scales = "free_y") +
    labs(y = NULL, x = xlab) +
    theme(legend.position = "none")
}


# token count differences ----

# step 1: get tokens counts matrix
tok_counts <- as.matrix(grouped_corp)
tok_counts[, 1:5]

# step 2: subtract counts between groups
diff_freq <- tok_counts["Lab", ] - tok_counts["Con", ]
diff_freq[1:4]

# visualize
stats <- get_top_n_terms(diff_freq)
plot_scores(x = stats, xlab = "Difference in word counts")


# token proportion differences ----

# step 1: compute total tokens per group
toks_per_group <- rowSums(tok_counts)

# step 2: compute proportions per group
tok_props <- tok_counts/toks_per_group

# step 3: subtract proportions between groups
diff_prop <- tok_props["Lab", ] - tok_props["Con", ]
diff_prop[1:4]

# visualize
stats <- get_top_n_terms(diff_prop)
plot_scores(x = stats, xlab = "Difference in word proportions")

# log odds ---

# add small constant to avoid zero proportions
eps <- 1e-12 
# clip proportions to avoid 0 or 1
tok_props_clipped <- pmin(pmax(tok_props, eps), 1 - eps)
# compute odds
odds <- tok_props_clipped / (1 - tok_props_clipped)
odds_ratio <- odds["Lab", ] / odds["Con", ]
log_odds <- log(odds_ratio)
log_odds[1:4]

stats <- get_top_n_terms(log_odds)
plot_scores(x = stats, xlab = "Log-odds ratio")


# smoothed log-odds ----

compute_logit <- function(group, e = 0.5) { 
  counts <- tok_counts[group, ]
  total  <- toks_per_group[group]
  log( (counts + e) / (total - counts + e) ) 
}
logit_L <- compute_logit("Lab")
logit_C <- compute_logit("Con")
log_odds_smooth <- logit_L - logit_C
log_odds_smooth[1:4]

stats <- get_top_n_terms(log_odds_smooth)
plot_scores(x = stats, xlab = "Log-odds ratio (smoothed)")


# Fightin' words ----
source(file.path("R", "fightin_words.R"))

fw_stats <- textstat_fighting_words(
  x = grouped_corp, 
  group.var = "Party", 
  .pairs = "permutations"
)

# look at example output
fw_stats[["Lab-Con"]] |> 
  select(feature, z_score, Party) |> 
  head(6)


# bring data into required format for plotting
top_zscores <- fw_stats[["Lab-Con"]] |> 
  # for each party group
  group_by(Party) |>
  # sort from highest to lowest
  arrange(desc(abs(z_score))) |> 
  # get top 20
  slice_head(n = 20) |> 
  ungroup()

# visualize
plot_scores(
  top_zscores,
  value.var = "z_score",
  group.var = "Party",
  xlab = "Fightin' words z-score"
)

