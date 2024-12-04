

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

source("~/surfdrive/VU Research Data Officer/FGB-RDO projects/Research support tools/rdm_webpage/rdm/code/functiondatatypesintext.R")



dataVCWE <- read.csv("~/Documents/Local_VU_docs/VCWE_registry/rawVCWEdata_total_tillend2019.csv", stringsAsFactors = FALSE)

depts <- dataVCWE %>% group_by(department) %>%
  count()

#Clinical developmental psychology

neuropsychdata <- dataVCWE %>% filter(str_detect(department, "Clinic") & str_detect(department, "Neuro") 
                                      & researchdata != "NA" & researchdata != "") %>%
  select(researchdata)

text_df <- cleanTextDf(neuropsychdata$researchdata)

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

themes_six <- sixgrams %>% filter(str_detect(ngram, "endocr") | str_detect(ngram, "neuro") | str_detect(ngram, "veroud") |
                                    str_detect(ngram, "aging") | str_detect(ngram, "activ") | str_detect(ngram, "cognit") |
                                    str_detect(ngram, "pedia") | str_detect(ngram, "verslav") | str_detect(ngram, "addic"))


datacoll_six <- sixgrams %>% filter(str_detect(ngram, "imag") | str_detect(ngram, "beelden") | str_detect(ngram, "mri") |
                                      str_detect(ngram, "neuro") | str_detect(ngram, "vuams") | str_detect(ngram, "amb") |
                                      str_detect(ngram, "meten") | str_detect(ngram, "meet") | str_detect(ngram, "measu") | 
                                      str_detect(ngram, "dagbo") | str_detect(ngram, "diar"))


