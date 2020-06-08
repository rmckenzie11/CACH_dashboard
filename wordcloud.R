library(SnowballC)
library(stringr)
library(tidyverse)
library(tm)
library(wordcloud)
library(RColorBrewer)

meetings <- readRDS("R Objects/transcripts.rds")
members <- readRDS("R Objects/members.rds")

meetings <- meetings[-7]
meetings <- tolower(meetings)

stopw <- tolower(c(members$last, stopwords("english"), "china", "committee","subcommittee","meeting","members","chair","opposition", "government", "motion", "canada", "canadians","important","minutes","briefings","parties","committees", "mr.", "think", "thank", "canadian", "chinese"))

for (i in 1:6) {
  meetings[i] = removeWords(meetings[i], stopw)
  meetings[i] = stripWhitespace(meetings[i])
  meetings[i] = paste(str_extract_all(meetings[i], '\\w{5,}')[[1]], collapse = ' ')
}

text <- meetings
dtm <- TermDocumentMatrix(Corpus(VectorSource(text)))

for (i in 1:6){
  dtm[i] <- TermDocumentMatrix(Corpus(VectorSource(meetings[i])))
}

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

v <- list()
d <- list()

for (i in 1:6){
  
  d[i] <- data.frame(word = names(v[i]),freq=v[i])
}

wordcloud(doc)
