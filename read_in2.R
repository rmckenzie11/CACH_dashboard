## Load Libraries

library(readtext)
library(tidyverse)
library(tidytext)
library(stringr)
library(sentimentr)
library(XML)

## Read in CACN raw text data
CACN <- readtext("Data/CACN.txt")

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

## for loop iterates through each meeting
for (i in 1:6) {
  colnames(comments[[i]]) <- "V1"

## Splits transcript into two columns, an ID and the text of their comment
  comments[[i]] <- comments[[i]]%>%
    separate(V1, into = c("name","text"), sep = ":", extra = "merge") 
  comments[[i]] <- na.omit(comments[[i]])
  comments[[i]]$name <- gsub("(\\((.*?)\\))","",comments[[i]]$name)
  comments[[i]]$name <- as.factor(trimws(gsub("of the Committee","",comments[[i]]$name)))
}

## Create intermediate step we can manipulate
mid <- comments

## New empty list for sentiments
sent_comments <- list()

## Sentiment Analysis
for (i in 1:6) {
  sent_comments[[i]] <- mid[[i]]$text %>% 
    sentiment() %>%
    filter(word_count > 2,
           abs(sentiment) < 1)
}

## Empty list for storing plots of sentiment analysis
dat <- list()
sent_plot_meetings <- list()

## For loop, creates 6 output plots, 1 for each meeting
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

## Bind all meetings together into one dataframe
all_comments <- bind_rows(mid, .id = "column_label")

## Sentiment analysis on all comments
sent_all_comments <- all_comments$text %>%
  sentiment_by()
all_comments <- all_comments %>%
  mutate(element_id = row_number())
sent_all_comments <- sent_all_comments %>%
  left_join(all_comments) %>%
  filter(!name %in% c("The Clerk", "The Chair"))

members <- xmlToDataFrame("Data/members.xml") %>%
  select(caucus = CaucusShortName, first = PersonOfficialFirstName, last = PersonOfficialLastName) 


members$caucus <- gsub("Ã©", "é", members$caucus)
members$first <- gsub("Ã©", "é", members$first)
members$last <- gsub("Ã©", "é", members$last)

## Create empty list
tidy_com <- list()

## Fill tidy_com with tidied text
for (i in 1:6) {
  mid[[i]]$text <-  gsub("[.]"," ", comments[[i]]$text)
  mid[[i]]$text <-  gsub("[?]"," ", comments[[i]]$text)
  tidy_com[[i]] <- mid[[i]] %>%
    unnest_tokens(word, text)
  tidy_com[[i]] <- tidy_com[[i]] %>%
    group_by(name) %>%
    tally(name = "count") 
}

tidy_com <- bind_rows(tidy_com, .id = "meeting") %>%
  group_by(meeting) %>%
  mutate(t = sum(count),
         per = count/t) %>%
  separate(name, into = c("honorific", "name"), sep = " ", extra = "merge") %>%
  separate(name, into = c("first", "last", sep = " ")) %>%
  left_join(members, by = c("first","last")) %>%
  filter(honorific != "The")

tidy_com$caucus <- tidy_com$caucus %>%
  replace_na("Witness")

tidy_com$caucus <- factor(tidy_com$caucus)

tidy_com <- tidy_com %>%
  mutate(fill = case_when(caucus == "Conservative" ~ "blue",
                          caucus == "Liberal" ~ "red",
                          caucus == "NDP" ~ "orange",
                          caucus == "Green Party" ~ "green",
                          caucus == "Witness" ~ "grey",
                          caucus == "Bloc Québécois" ~ "purple"))

tidy_com <- split(tidy_com , f = tidy_com$meeting)


talk_plot_meetings <- list()

for (i in 1:6) {
  tidy_com[[i]]$fill <- factor(tidy_com[[i]]$fill, levels = c("red", "blue", "orange", "purple", "green", "grey"))
  talk_plot_meetings[[i]] <- ggplot(tidy_com[[i]], aes(reorder(last, as.numeric(fill)), count, fill = fill)) +
    geom_bar(stat = "identity", width = 0.5, color = "black") +
    coord_flip() +
    scale_fill_identity(guide = "legend", labels = tidy_com[[i]]$caucus, breaks = tidy_com[[i]]$fill) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(title = "# of Words Spoken in Committee by MP or Witness") +
    xlab("Name") +
    ylab("# of Words")
}

talk_plot_meetings_mp <- list()

for (i in 1:6) {
  tidy_com[[i]]$fill <- factor(tidy_com[[i]]$fill, levels = c("red", "blue", "orange", "purple", "green", "grey"))
  df <- tidy_com[[i]] %>%
    filter(caucus != "Witness")
  talk_plot_meetings_mp[[i]] <- ggplot(df, aes(reorder(last, as.numeric(fill)), count, fill = fill)) +
    geom_bar(stat = "identity", width = 0.5, color = "black") +
    coord_flip() +
    scale_fill_identity(guide = "legend", labels = reorder(tidy_com[[i]]$caucus, as.numeric(tidy_com[[i]]$fill)), breaks = reorder(tidy_com[[i]]$fill, as.numeric(tidy_com[[i]]$fill))) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(title = "# of Words Spoken in Committee by MP") +
    xlab("Name") +
    ylab("# of Words")
}

## Send the prepared data to Shiny!

saveRDS(all_comments, "all_comments.rds")
saveRDS(tidy_com, "tidy_words.RDS")
saveRDS(sent_plot_meetings,"sent_plot_meetings.rds")
saveRDS(talk_plot_meetings,"talk_plot_meetings.rds")
saveRDS(members, "members.rds")
saveRDS(meetings, "transcripts.rds")

saveRDS(talk_plot_meetings_mp, "talk_plots_mp.rds")

