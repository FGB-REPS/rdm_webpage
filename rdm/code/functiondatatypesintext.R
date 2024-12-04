
#guidance on text cleaning and manipulation is found on these pages:
#https://www.tidytextmining.com/tidytext.html
#https://paldhous.github.io/NICAR/2019/r-text-analysis.html


library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(stopwords)
library(tm)

cleanTextDf <- function(x) {
  dfCorpus <- SimpleCorpus(VectorSource(x))
  
  #following steps clean up the text to allow for analysis; the methods with tm_map and use of a "corpus" work better than the
  # options available through tidyverse
  
  dfCorpus <- tm_map(dfCorpus, stripWhitespace)
  dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
  dfCorpus <- tm_map(dfCorpus, removeNumbers)
  dfCorpus <- tm_map(dfCorpus, removePunctuation)
  dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))
  dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("dutch"))
  
  #turn the dfCorpus back into a vector and then a tibble
  text <- c()
  for (i in 1:length(dfCorpus)) {
    text <- c(text, dfCorpus[[i]]$content)
  }
  
  text_df <- tibble(line = 1:length(text), text = text)

  
}




#text_df per word

#to write functions with dplyr see this page https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
#this is required because %>% in dplyr require extra steps to be able to "see" the correct variable

textWords <- function(x, var) {
  input <- enquo(var)
  x %>%
    unnest_tokens(word, !! input) %>%
    group_by(word) %>%
    count() %>%
    arrange(desc(n))
}



textWordsMult <- function(x, var, num) {
  input <- enquo(var)
  x %>%
    unnest_tokens(ngram, !! input, token = "ngrams", n = num) %>%
    group_by(ngram) %>%
    count() %>%
    arrange(desc(n))
}


