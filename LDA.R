library(topicmodels)
library(tidytext)
library(readtext)
library(stringr)
library(tidyverse)
library(tm)

CACN <- readtext("CACN.txt")
CACN$text <- gsub("\\\\xa0", "", CACN$text)
CACN$text <- gsub("\"", "", CACN$text)
CACN$text <- gsub(", \'", ".", CACN$text)
CACN$text <- gsub("\'", "", CACN$text)

CACN$text <- gsub("\\\\n", "skipskip", CACN$text)
CACN$text <- gsub("skipskipskipskipskipskipskipskip", "", CACN$text)

CACN_data <- str_split(CACN$text, "adjourned")

meetings1 <- CACN_data[[1]][1]

comments1 <- str_split(meetings1, "skipskip")

y <- data.frame(comments1[[1]])
colnames(y) <- "V1"

com1 <- y %>%
  separate(V1, into = c("name","text"), sep = ":", extra = "merge")

com1 <- na.omit(com1)
com1$name <- gsub("(\\((.*?)\\))","",com1$name)
com1$name <- as.factor(trimws(gsub("of the Committee","",com1$name)))

com1$text = removeWords(com1$text, stopwords("english"))
com1$text = stripWhitespace(com1$text)

myCorpus <- Corpus(VectorSource(com1$text))
tdm <- DocumentTermMatrix(myCorpus)

com1_lda <- LDA(tdm, k = 2, control = list(seed = 1010))

com1_topics <- tidy(com1_lda, matrix = "beta")

top_terms <- com1_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
