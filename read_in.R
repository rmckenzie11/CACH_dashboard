library(readtext)
library(tidyverse)
library(tidytext)
library(stringr)

CACN <- readtext("CACN.txt")

## Clean wierd HTML text
CACN$text <- gsub("\\\\xa0", "", CACN$text)
CACN$text <- gsub("\"", "", CACN$text)
CACN$text <- gsub(", \'", ".", CACN$text)
CACN$text <- gsub("\'", "", CACN$text)

## Replace new line sign with skip to avoid having to be careful with backslashes
CACN$text <- gsub("\\\\n", "skipskip", CACN$text)
CACN$text <- gsub("skipskipskipskipskipskipskipskip", "", CACN$text)

## Split data by meeting
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

tidy_com <-  com1 %>%
  unnest_tokens(word, text)



tot <- tidy_com %>%
  count(name)

sent <- tidy_com %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment,name) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  inner_join(tot)

transcript <- data.frame(meetings1) %>%
  mutate(text = as.character(meetings1))

sent_meeting <- transcript %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


saveRDS(sent_meeting,"sent_meeting1.rds")
saveRDS(sent, "sent_MP.rds")
saveRDS(tidy_com, "tidy_words.RDS")
