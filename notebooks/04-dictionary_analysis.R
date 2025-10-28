# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Code from slides `04-dictionary_analysis.pdf`
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2025-10-21
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #


library(quanteda)
data("data_dictionary_LSD2015", package = "quanteda")
class(data_dictionary_LSD2015)
names(data_dictionary_LSD2015)


# TODO: renv::install("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
data(package="quanteda.sentiment")





data("data_corpus_ukmanifestos", package = "quanteda.corpora")
dtm <- data_corpus_ukmanifestos |> 
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE) |> 
  as.tokens_xptr() |>
  tokens_tolower() |> 
  # lemmatize (based on https://stackoverflow.com/a/62330539)
  tokens_replace(
    pattern = lexicon::hash_lemmas$token, 
    replacement = lexicon::hash_lemmas$lemma
  ) |> 
  dfm()

data("data_dictionary_LSD2015", package="quanteda.sentiment")

dtm_scored <- dfm_lookup(
  dtm, 
  dictionary = data_dictionary_LSD2015, 
  valuetype = "glob" # beacuse the dictionary uses globs (wildcards)
)

# result just contains columns ("features") recording
#  count of terms matching a given dictionary category
dtm_scored


library(dplyr)

# get document variables
doc_metadata <- docvars(dtm_scored)
doc_metadata$doc_id <- docnames(dtm_scored)

scores_df <- dtm_scored |> 
  convert("data.frame", docid_field = "doc_id") |> 
  left_join(doc_metadata, by = "doc_id") |> 
  mutate(
    total_pos = positive+neg_negative,
    total_neg = negative+neg_positive,
    # see formula (1) in Proksch et al. (2019)
    sentiment = log( (total_pos+0.5) / (total_neg+0.5) )
  )


scores_df <- scores_df |> 
  # subset
  filter(
    Country=="UK",
    Type=="natl",
    Language=="en",
    Party %in% c("Con", "Lab", "Lib", "LibSDP", "LD", "DUP", "SNP")
  ) |> 
  transmute(
    # recode the LibDems party label
    party = ifelse(
      Party %in% c("Lib", "LibSDP", "LD"), 
      "Lib", 
      Party
    ),
    year = as.integer(Year),
    sentiment
  ) 


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



party_colors_map <- c(
  "Con" = "#0087DC",
  "Lab" = "#E4003B",
  "Lib" = "#FAA61A",
  "DUP" = "#D46A4C",
  "SNP" = "#FFFF00"
)

scores_df |> 
  ggplot(aes(x=party, y=sentiment, fill=party)) +
  geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) + 
  scale_fill_manual(breaks = names(party_colors_map), values=party_colors_map) +
  labs(x=NULL) + 
  theme(legend.position="none")



# asses gov't opposition differences
sitting_governments_at_elections <- tribble(
  ~year, ~sitting_government,
  1945, "Lab",
  1950, "Lab",
  1951, "Lab", # Con won  election
  1955, "Con",
  1959, "Con",
  1964, "Con", # Lab won election
  1966, "Lab",
  1970, "Lab", # Con won election
  1974, "Con", # Lab won election
  1979, "Lab", # Con won election
  1983, "Con",
  1987, "Con",
  # 1990, "Con",
  1992, "Con",
  1997, "Con", # Lab won election
  2001, "Lab",
  # 2003, "Lab",
  2005, "Lab",
)

scores_df |> 
  filter(party %in% c("Con", "Lab")) |> 
  left_join(sitting_governments_at_elections, by="year") |>
  mutate(status = ifelse(party == sitting_government, "Government", "Opposition")) |> 
  ggplot(aes(x=status, y=sentiment, fill=party)) +
  geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) + 
  scale_fill_manual(breaks = names(party_colors_map), values=party_colors_map) +
  labs(x=NULL) +
  facet_wrap(~party) + 
  theme(legend.position = "none")



fp <- file.path("data", "dictionaries", "anew2010", "ANEW2010All.txt")
anew_scores <- readr::read_tsv(fp, col_select = c("Word", "ValMn"))
colnames(anew_scores) <- c("term", "valence")
# NOTE: valence = sentiment


scored <- anew_scores |> 
  corpus(text_field = "term") |> 
  tokens() |> 
  dfm() |> 
  dfm_lookup(data_dictionary_LSD2015, valuetype = "glob") |> 
  convert(to = "data.frame")

bind_cols(scored, anew_scores) |> 
  filter(negative + positive > 0) |> 
  mutate(
    category = ifelse(negative == 1, "negative", "positive")
  ) |> 
  ggplot(aes(x = category, y = valence, fill = category)) +
    geom_boxplot(alpha = 0.8, color = "grey", outliers = FALSE) +
    labs(
      x = "category in LSD dictionary",
      y = "ANEW valence score"
    ) +
    ylim(0, 10) +
    theme(legend.position = "none")


