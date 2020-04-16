library(topicmodels)
library(tidytext)
library(readtext)
library(stringr)
library(tidyverse)
library(tm)
library(XML)

members <- xmlToDataFrame("Data/members.xml") %>%
  select(caucus = CaucusShortName, first = PersonOfficialFirstName, last = PersonOfficialLastName) 


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

stopw <- tolower(c(members$last, stopwords("english"), "china", "committee","subcommittee","meeting","members","chair","opposition", "government", "motion", "canada", "canadians","important","minutes","briefings","parties","committees", "mr.", "think", "thank", "canadian", "chinese"))

for (i in 1:6) {
  meetings[i] = removeWords(meetings[i], stopw)
  meetings[i] = stripWhitespace(meetings[i])
  meetings[i] = paste(str_extract_all(meetings[i], '\\w{5,}')[[1]], collapse = ' ')
}


temp <- Corpus(VectorSource(meetings)



c <- DocumentTermMatrix(Corpus(VectorSource(meetings)))
lda <- LDA(c, k = 6, control = list(seed = 1010))

topics <- tidy(lda, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

LDA <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

saveRDS(LDA, "LDA.rds")

