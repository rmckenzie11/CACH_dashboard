library(topicmodels)
library(tidytext)
library(readtext)
library(stringr)
library(tidyverse)
library(tm)
library(cleanNLP)

## Unfinished Natural text processing

#reticulate::use_python("C:/Users/Robert/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Python 3.8/Python 3.8 (32-bit).lnk")
#cnlp_init_spacy()

#cnannotatedText <- cnlp_annotate(gospels, as_strings = TRUE,
#                                   text_var = "text", doc_var = "verse_title")


## Read in dataset of MPs
members <- readRDS("Data/members.rds") 


members$caucus <- gsub("Ã©", "é", members$caucus)
members$first <- gsub("Ã©", "é", members$first)
members$last <- gsub("Ã©", "é", members$last)

## Read in CACN raw text data
CACN <- readtext("Data/CACN.txt")

## Replace new line sign with skip to avoid having to be careful with backslashes
CACN$text <- gsub("\\\\n\\\\xa0\\\\n", " ", CACN$text)

## Clean wierd HTML text
CACN$text <- gsub("\\\\xa0", " ", CACN$text)
CACN$text <- gsub("\"", " ", CACN$text)
CACN$text <- gsub(", \'", ".", CACN$text)
CACN$text <- gsub("\'", " ", CACN$text)

## Split data by meeting
CACN_data <- str_split(CACN$text, "adjourned")

## Pull out just the text
meetings <- CACN_data[[1]]
meetings <- meetings[-7]
meetings <- tolower(meetings)

## Define common stopwords
stopw <- tolower(c(members$last, stopwords("english"), "china", "committee","subcommittee","meeting","members","chair","opposition", "government", "motion", "canada", "canadians","important","minutes","briefings","parties","committees", "mr.", "think", "thank", "canadian", "chinese"))


## Clean text
for (i in 1:6) {
  meetings[i] = removeWords(meetings[i], stopw)
  meetings[i] = stripWhitespace(meetings[i])
  meetings[i] = paste(str_extract_all(meetings[i], '\\w{5,}')[[1]], collapse = ' ')
}

## Run LDA on cleaned text
c <- DocumentTermMatrix(Corpus(VectorSource(meetings)))
lda <- LDA(c, k = 6, control = list(seed = 1010))


## Plot LDA
topics <- tidy(lda, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

saveRDS(lda_plot, "LDA.rds")

