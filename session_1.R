install.packages("quanteda")
library("quanteda")
remotes::install_github("quanteda/quanteda.corpora")
library("quanteda.corpora")
remotes::install_github("quanteda/quanteda.textstats")
library("quanteda.textstats")


data("data_corpus_ukmanifestos", package = "quanteda.corpora")

corp_dta <- corpus(data_corpus_ukmanifestos)
tok_dta <- tokens(corp_dta)
dfm_dta <- dfm(tok_dta)
print(dfm_dta)

ndoc(dfm_dta)
docvars(dfm_dta)

min(ntoken(dfm_dta))
max(ntoken(dfm_dta))
mean(ntoken(dfm_dta))

sparsity(dfm_dta)

textstat_frequency(dfm_dta, 10)
freq <- textstat_frequency(dfm_dta)
tail(freq, 10)

toks_clean <- corpus(data_corpus_ukmanifestos) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem() %>%
  tokens_tolower()
dfm_clean <- dfm(toks_clean)
sparsity(dfm_clean)

# alternative (punct_remove fehlt, nur in "tokens"-Funktion möglich)
dfm_clean <- dfm(tok_dta,
                 tolower = T,
                 ) %>%
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_wordstem() %>%
  dfm_trim(min_termfreq = 5, max_docfreq = 0.8, docfreq_type = "prop")

sparsity(dfm_clean)

# combination: lowercasing, stemming, trimming (not punctuation removal and "stop words" removal

dfm_9 <- dfm_trim(dfm_clean, min_docfreq = 0.9, docfreq_type = "prop")

dfm_5 <- dfm_trim(dfm_clean, max_termfreq = 5, docfreq_type = "prop")

topfeatures(dfm_9)
topfeatures(dfm_5)













