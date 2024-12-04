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

#biological psychology

biopsychdata <- dataVCWE %>% filter(str_detect(department, "Biolog") & str_detect(department, "Psych") 
                                    & researchdata != "NA" & researchdata != "") %>%
  select(researchdata)

# barely any submissions probably due to overarching VCWE submission. All about questionnaires. Nothing new to report.