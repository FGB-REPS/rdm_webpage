

library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(stopwords)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(RCurl)

#upload the functions for cleaning the text and making word counts

source("~/surfdrive/fgb_research_data_steward/1.projects/support_docs_webpages/rdm_webpage/rdm/code/functiondatatypesintext.R")



dataVCWE <- read.csv("~/Documents/Local_VU_docs/VCWE_registry/rawVCWEdata_total_tillend2019.csv", stringsAsFactors = FALSE)

depts <- dataVCWE %>% group_by(department) %>%
  count()

#Clinical developmental psychology

clinpsychdata <- dataVCWE %>% filter(str_detect(department, "Clinical Psy") 
                                      & researchdata != "NA" & researchdata != "") %>%
  select(researchdata)

text_df <- cleanTextDf(clinpsychdata$researchdata)

#remove words that are repeated multiple times in one record (this didn't want to work within the function cleanTextDF)
for (i in 1:nrow(text_df)) {
  text_df$text[i] <- paste(unique(unlist(str_split(text_df$text[i]," "))), collapse = " ")
}


text_words <- textWords(text_df, text)

#following code can be used to make a word cloud-it doesn't work so well here because there aren't a lot a overlapping words

#wordcloud(words = text_words$word, freq = text_words$n, min.freq = 100,
#          max.words=100, random.order=FALSE, rot.per=0.35, 
#          colors=brewer.pal(8, "Dark2"))

bigrams <- textWordsMult(text_df, text, 2)

#less useful to make word cloud here, too much is dropped


trigrams <- textWordsMult(text_df, text, 3)

#less useful to make word cloud here, too much is dropped



#not a lot of overlap in words, bigrams or trigrams. Look at longer blocks of text and select based on themes/data
#collection method

sixgrams <- textWordsMult(text_df, text, 6)

#not a lot of themes that can say much unique about data collection like with Neuro Psych or Devel Psych. Will focus 
#possible data collection modi.


datacoll_six <- sixgrams %>% filter(str_detect(ngram, "imag") | str_detect(ngram, "beelden") | str_detect(ngram, "mri") |
                                      str_detect(ngram, "neuro") | str_detect(ngram, "vuams") | str_detect(ngram, "amb") |
                                      str_detect(ngram, "meten") | str_detect(ngram, "meet") | str_detect(ngram, "measu") | 
                                      str_detect(ngram, "dagbo") | str_detect(ngram, "diar") | str_detect(ngram, "netw") |
                                      str_detect(ngram, "crf") | str_detect(ngram, "clinical rep") | 
                                      str_detect(ngram, "interview") | str_detect(ngram, "gesprek"))



