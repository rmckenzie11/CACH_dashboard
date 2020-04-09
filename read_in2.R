library(readtext)
library(tidyverse)
library(tidytext)
library(stringr)
library(sentimentr)

CACN <- readtext("CACN.txt")

## Replace new line sign with skip to avoid having to be careful with backslashes
CACN$text <- gsub("\\\\n\\\\xa0\\\\n", "skip", CACN$text)

## Clean wierd HTML text
CACN$text <- gsub("\\\\xa0", "", CACN$text)
CACN$text <- gsub("\"", "", CACN$text)
CACN$text <- gsub(", \'", ".", CACN$text)
CACN$text <- gsub("\'", "", CACN$text)

## Split data by meeting
CACN_data <- str_split(CACN$text, "adjourned")

## Pull out just the text
meetings <- CACN_data[[1]]

## Split text corpus into strings by new lines.
comments <- str_split(meetings, "skip")

## Remove extra last list
comments <- comments[-7]

## Convert list of text corpuses to list of dataframes
comments <- lapply(comments, data.frame)

## "for loop" maybe remove, rename columns
for (i in 1:6) {
  colnames(comments[[i]]) <- "V1"
}

## another for loop, will try lapply later, splits dataframes into columns of name and comment, then does some cleaning
for (i in 1:6) {
  comments[[i]] <- comments[[i]]%>%
    separate(V1, into = c("name","text"), sep = ":", extra = "merge") 
  comments[[i]] <- na.omit(comments[[i]])
  comments[[i]]$name <- gsub("(\\((.*?)\\))","",comments[[i]]$name)
  comments[[i]]$name <- as.factor(trimws(gsub("of the Committee","",comments[[i]]$name)))
}

## Create empty list
tidy_com <- list()
mid <- comments

## Fill tidy_com with tidied text
for (i in 1:6) {
  
  mid[[i]]$text <-  gsub("[.]"," ", comments[[i]]$text)
  tidy_com[[i]] <- mid[[i]] %>%
    unnest_tokens(word, text)
  
}

sent_comments <- list()

for (i in 1:6) {
  sent_comments[[i]] <- mid[[i]]$text %>% 
    sentiment() %>%
    filter(word_count > 2,
           abs(sentiment) < 1)
}

dat <- list()
sent_plot_meetings <- list()
for(i in 1:6) {
  
dat[[i]] <- with(density(sent_comments[[i]]$sentiment), data.frame(x, y))

sent_plot_meetings[[i]] <- ggplot(dat[[i]], aes(x = x, y = y)) +
  geom_line() +
  geom_area(mapping = aes(x = ifelse(x >=0 & x<=1 , x, 0)), fill = "green") +
  geom_area(mapping = aes(x = ifelse(x <=0 & x>=-1 , x, 0)), fill = "red") +
  scale_y_continuous(limits = c(0,7.5)) +
  theme_minimal(base_size = 16) +
  labs(x = "Sentiment", 
       y = "", 
       title = "Distribution of Sentiment Across Meeting") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.y=element_blank()) 
}

all_comments <- bind_rows(comments, .id = "column_label")

  




saveRDS(tidy_com, "tidy_words.RDS")
saveRDS(sent_plot_meetings,"sent_plot_meetings.rds")
