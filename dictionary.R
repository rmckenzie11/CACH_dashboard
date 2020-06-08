library(tm)
library(tidyverse)
library(wesanderson)
library(gganimate)
sent_all_comments <- readRDS("CACN_dash/sent_all_comments.rds")
tidy_words <- readRDS("CACN_dash/tidy_words.RDS")


dict <- data.frame(trade= rep(0,11))
dict["trade"] <- c("trade", "commerce", "exchange", "sell", "export", "import", "market", "trading", "profit", "pork", "canola")
dict["health"] <- c("disease", "health", "illness", "covid", "coronavirus", "virus", "pandemic", rep(NA,4))
dict["consular"] <- c("consul", "kovrig", "spavor", rep(NA,8))
dict["rights"] <- c("right", "civil", "legal", "freedom", "feminist", "women", rep(NA,5))
dict["5g"] <- c("huawei", "5G", "tech", "communication", rep(NA,7))
dict["free press"] <- c("press", "news", "magazine", "journal", "journalist", "report", "reporter", "truth", "censor", "media", NA)
dict["Hong Kong"] <- c("hong", "suffrage", "protest", "two systems", rep(NA,7))
dict["Taiwan"] <- c("taiwan", rep(NA,10))

dict <- as.data.frame(apply(dict,2,function(x)gsub('_', ' ',x)))

dict <- dict %>%
  pivot_longer(cols = 1:8, names_to = "topic") %>%
  drop_na() %>%
  filter(topic != "X1") %>%
  mutate(value = as.character(value))

dict2 <- tolower((stemDocument(setNames(as.character(dict$value), dict$topic))))
dict2 <- dict2[!duplicated((dict2))]

mp_topic <- list()
tidy_words2 <- bind_rows(tidy_words, .id = "meeting")

for (i in 1:43){
  mp_topic[[i]] <- list()
  dat <- tidy_words2 %>%
    filter(name == unique(sent_all_comments$name)[i]) %>%
    mutate(word = stemDocument(word))
  for(word in dat$word){
    mp_topic[[i]] <- append(mp_topic[[i]], names(dict2)[dict2 == word])
  }
}

names(mp_topic) <- unique(sent_all_comments$name)


topic <- list()
tidy_words_mp <- list()
topic_mp <- list()

for(i in 1:6) {
 tidy_words_mp[[i]] <- tidy_words[[i]] %>%
   separate(name, into = c("honorific", "name"), sep = " ", extra = "merge") %>%
   separate(name, into = c("first", "last", sep = " ")) %>%
  filter(last %in% members$last) %>%
   select(last, word)
 topic_mp[[i]] <- list()
 for(word in tidy_words_mp[[i]]$word){
   topic_mp[[i]] <- append(topic_mp[[i]], names(dict2)[dict2 == word])
 }
}

for (i in 1:6){
  topic[[i]] <- list()
  for(word in tidy_words[[i]]$word){
    topic[[i]] <- append(topic[[i]], names(dict2)[dict2 == word])
  }
}

meeting_top_plots <- list()
meeting_top_plots_mp <- list()

for (i in 1:6){
  t <- data.frame(topic = unlist(topic_mp[[i]])) %>%
    mutate(fill = case_when(topic == "trade" ~ "#606c38",
                            topic == "health" ~ "#283618",
                            topic == "rights" ~ "#DA9587",
                            topic == "free press" ~ "#dda15e",
                            topic == "consular" ~ "#bc6c25",
                            topic == "Taiwan" ~ "#5e6472",
                            topic == "Hong Kong" ~ "#9a8c98",
                            TRUE ~ "black"))
  meeting_top_plots_mp[[i]] <- ggplot(t, aes(y = topic, fill = fill)) +
    geom_bar() +
    theme_classic() +
    scale_fill_identity()
}


for (i in 1:6){
  t <- data.frame(topic = unlist(topic[[i]])) %>%
    mutate(fill = case_when(topic == "trade" ~ "#606c38",
                            topic == "health" ~ "#283618",
                            topic == "rights" ~ "#DA9587",
                            topic == "free press" ~ "#dda15e",
                            topic == "consular" ~ "#bc6c25",
                            topic == "Taiwan" ~ "#5e6472",
                            topic == "Hong Kong" ~ "#9a8c98",
                            TRUE ~ "black"))
  meeting_top_plots[[i]] <- ggplot(t, aes(y = topic, fill = fill)) +
    geom_bar() +
    theme_classic() +
    scale_fill_identity()
}

all_meetings <- data.frame()
for(i in 1:6){
  t <- data.frame(topic = unlist(topic[[i]]))
  t <- t %>%
    count(topic) %>%
    mutate(meeting = i)
  all_meetings <- rbind(all_meetings,t)
}

all_meetings <- all_meetings %>%
  group_by(meeting) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(meeting = case_when(meeting == 1 ~ 1,
                              meeting == 2 ~ 3,
                              meeting == 3 ~ 4,
                              meeting == 4 ~ 5,
                              meeting == 5 ~ 7,
                              meeting == 6 ~ 8))



ggplot(all_meetings, aes(x = meeting, y = prop, color = topic)) +
  geom_line(size = 2)

anim1 <- ggplot(all_meetings, aes(x = topic,y  = prop, fill = topic)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values=c(wes_palette("GrandBudapest1"),wes_palette("IsleofDogs1"))) +
  labs(title = "Meeting: {closest_state}") +
  transition_states(meeting, transition_length = 1, state_length = 5)

saveRDS(anim1, "anim1.rds")
saveRDS(mp_topic, "topics_by_mp.rds")
saveRDS(meeting_top_plots, "topics_by_meeting.rds")
saveRDS(meeting_top_plots_mp, "topics_by_meeting_mp.rds")
