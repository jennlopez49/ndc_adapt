

library(rvest) 
library(tm) 
library(tidytext)
library(tinytex)
library(topicmodels)
library(stm)

ndc <- Corpus(DirSource('dataverse_files/NDC_corpus'), 
              readerControl=list(reader=readPlain)) 

ndc_words <- ndc %>% tm_map(stripWhitespace)
ndcs <- tm_map(ndc_words, tolower)

ndcs <- tm_map(ndcs, removeWords, stopwords("english"))
# ndcs <- tm_map(ndcs, removeWords, stopwords("spanish"))   
# ndcs <- tm_map(ndcs, removeWords, stopwords("french"))

ndcs <- tm_map(ndcs, removePunctuation)
ndcs <- tm_map(ndcs, removeNumbers)

# function to remove bullets 
clean_text <- function(text) {
  text <- tolower(text)  # Convert to lowercase (optional)
  text <- gsub("[[:digit:]]|•", "", text)  # Remove numbers and • character
  text <- removePunctuation(text)  # Remove other punctuation
  return(text)
}

no_text <- function(text) {
  text <- tolower(text)  # Convert to lowercase (optional)
  text <- gsub("[[:space:]]", "", text)  # Remove numbers and • character
  text <- removePunctuation(text)  # Remove other punctuation
  return(text)
}

ndcs <- tm_map(ndcs, content_transformer(clean_text))
ndcs_cleaned <- tm_map(ndcs, content_transformer(no_text))

dtm <- DocumentTermMatrix(ndcs)

findFreqTerms(dtm, 500)

findAssocs(dtm, "adaptation", 0.5)

ndcs_model <-  LDA(dtm, k=2,  control = list(seed=1234))

ndc_topics <- tidy(ndcs_model, matrix = "beta")
ndc_topics
