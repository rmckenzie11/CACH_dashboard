library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)

tr <- readRDS("sent_meeting1.rds") %>%
  mutate(emote = positive + negative)
mp <- readRDS("sent_MP.rds") %>%
  mutate(emote = positive + negative) %>%
  filter(n > 30)
tidy_words <- readRDS("tidy_words.RDS")

common_words <- tidy_words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  tail(1619)
