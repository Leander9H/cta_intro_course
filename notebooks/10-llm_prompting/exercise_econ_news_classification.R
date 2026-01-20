# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Exercise on LLM prompting for text classification
#' @course VU 402150 "Intro to Computational Text Analysis with R"
#' @author Hauke Licht
#' @date   2026-01-20
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

## libraries ----
library(readr)
library(dplyr)
library(ellmer)
library(yardstick)
library(usethis)

## data ----

# NOTE: assuming you run this file in th context of our courses R project
fp <- file.path("data", "labeled", "barbera_automated_2021", "barbera_automated_2021-econ_topic.csv")

# load the CSV file
df <- read_csv(fp)

# NOTE: the label categories are "yes" (about economy) and "no" (not about economy)
df$label <- factor(df$label, c("yes", "no"), c("economy", "other"))


## create data splits ----

# NOTE: set random number generation seed for reproducibility
set.seed(1234) 

# randomly assign 20% of documents to test split
df$metadata__split <- sample(
  size = nrow(df), 
  x = c("train", "test"), 
  prob = c(0.8, 0.2),
  replace = TRUE 
)

df |> 
  with(table( metadata__split, label)) |> 
  prop.table(1) |>
  round(3)

#### SETUP MODEL ####
model_id <- "Qwen/Qwen3-Next-80B-A3B-Instruct:together"
# create a hugging face backend
model <- chat_huggingface(
  model = model_id,
  params = params(
    temperature = 0.0, 
    seed = 42
  )
)
class(model)

model$chat("What is the capital of Austria? Respond in one sentence.", echo = "none")
model$set_turns(list())

#### TRYOUT ####
instructions <- 
  "You will be provided with a text.

Your task is to classify the text's sentiment using the categories positive, negative, or neutral.

Only respond with one of the allowed labels: positive, negative, neutral"

# set the system message of the model
model$set_system_prompt(instructions)

response_format <- type_enum(
  values = c("positive", "neutral", "negative"), 
  description = "Sentiment classification of the input text"
)

# specify the text to be classified as user message
text <- "I love programming in R!"

# send the text to the model
model$chat(text)
model$set_turns(list())

# generate a structured response 
model$chat_structured(text, type = response_format)
model$set_turns(list())

# NOTE: we re-use the instructions defined above
model$set_system_prompt(instructions)

texts <- c(
  "I love programming in R!",
  "Learning text analysis with Hauke is fun!",
  "But I hate bugs in my code.",
  "Oh well, debugging is okay, I guess."
)

parallel_chat_structured(
  model,
  prompts = as.list(texts), # NOTE: important to convert to list
  type = response_format # NOTE: we re-use the response format defined above
)
model$set_turns(list())

# NEW, MORE COMPLEX
response_format <- type_object(
  reasoning = type_string(
    description = "Your reasoning of what sentiment category should be assigned to the text"
  ),
  category = type_enum(
    c("positive", "neutral", "negative"), 
    description = "The sentiment category you assign to the text"
  ),
  .description = "Sentiment classification with reasoning"
)

model$set_system_prompt(instructions)

text <- "I love programming in R!"

# generate a structured response 
model$chat_structured(text, type = response_format)
model$set_turns(list())

# instruction
# response format
# user text
# assistant (= response)

# TODO ----

#' 1. Define task instructions for the classification of news headlines into 
#'    _economic news_ and other topics
#'    hint: apply the best practices described in the course slides
instructions <- 
  "You will be provided with a text.

Your task is to classify the text's headlines _economic news_ and other topics.

Only respond with one of the allowed labels: _economic news_ and _other topics_"

# set the system message of the model
model$set_system_prompt(instructions)

#' 2. Define an appropriate response format.
response_format <- type_object(
  reasoning = type_string(
    description = "Your reasoning of what sentiment category should be assigned to the text"
  ),
  category = type_enum(
   c("economic", "other"), 
    description = "The headline category you assign to the text."
  ),
  .description = "Sentiment classification with reasoning"
)

#' 3. Setup your LLM using the Hugging Face API token and the model  
#' 4. Test your prompt on a few examples from the training set
texts <- c(
  "Pain ahead as Britons braced for 11% inflation",
  "Democrats seek to remove Trump after 'darkest day'",
  "BROOKLYN BLASTS POSH & BECKS",
  "RBI guv: Monetary policy has limits, fiscal steps needed too"
)

parallel_chat_structured(
  model,
  prompts = as.list(texts), # NOTE: important to convert to list
  type = response_format # NOTE: we re-use the response format defined above
)
model$set_turns(list())

#' 5. Refine your task instructions, if needed
#' 
#' 6. Apply your LLM prompt to the test set and collect predictions
#' 
#' 7. Evaluate the classification performance using appropriate metrics
#'    hint: refer to the materials for our session on supervised classification
#'    on how to evaluate classifiers (especially: precision, recall, F1)
