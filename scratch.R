
stem_hunspell <- function(term) {
  # look up the term in the dictionary
  stems <- hunspell::hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- term
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  
  stem
}


x <- sent_all_comments %>%
  select(name, text, column_label)

x <- x %>%
  mutate(text = gsub("\\.", " ", text),
         text = gsub(",", " ", text),
         text = gsub("-", " ", text))

y <- tokens(x$text, remove_punct = TRUE)

names(y) <- x$name

z <- unlist(y)

dat <- data.frame(name = names(z), word = z)

for(i in 1:6){
  df <- x %>%
    filter(column_label == i)
 print(length(unlist(tokens(df$text, remove_punct = TRUE))))
}

dat$name <- gsub("\\.[^.]*$","",dat$name)


dat$docname <- c(rep(1, 12177), rep(3, 15851), rep(4, 15959), rep(5, 17678), rep(7, 23734), rep(8, 23995))

df <- dat %>%
  filter(str_length(word) > 5) %>%
  mutate(word = tolower(word),
         word = unlist(text_tokens(word, stemmer = stem_hunspell))) %>%
  filter(!word %in% c("canada", "china", "committee", "mote", "meet", "subcommittee", "member", "brief", "govern"),
         str_length(word) > 5) %>%
  drop_na() %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n))

stopwords <- head(df, 15)

dat <- dat %>%
  filter(str_length(word) > 5) %>%
  mutate(word = tolower(word),
         word = unlist(text_tokens(word, stemmer = stem_hunspell))) %>%
  filter(!word %in% c("canadians", "chinese", "canadian", "should", "question", "subcommittee", "member", "brief", "govern", "canada", "china", "committee", "official", "minister", "witness", "minute", "parliament", "parliamentary", "country", "important")) 

saveRDS(dat, "tokenized_comments.rds")


dat2 <- dat %>%
  separate(name, into = c("honorific", "name"), sep = " ", extra = "merge") %>%
  separate(name, into = c("first", "last", sep = " ")) %>%
  left_join(members, by = c("first","last")) %>%
  filter(honorific != "The") %>%
  select(-` `)

dat2 <- dat2 %>%
  drop_na() 

dat2$name <- paste(dat2$honorific, dat2$first, dat2$last)

saveRDS(dat2, "just_mp.rds")

docs <- sent_all_comments

temp <- aggregate(docs$text, list(docs$column_label), paste, collapse=" ")

t <- corpus(temp, docid_field = "Group.1", text_field = "x")

freq <- dfm(t) %>%
  dfm_group(groups = c("1", "2", "3", "4", "5","6")) %>%
  dfm_tfidf() 

dat3 <- as.data.frame(t(freq))

names(dat3) <- c("word", "doc1", "doc3", "doc4", "doc5", "doc7", "doc8")

meeting_tf <- list()
meeting_tf_plots <- list()
fills <- c(wes_palette("Darjeeling1"), wes_palette("BottleRocket1"))


for(i in 1:6){
  temp = as.character(names(dat3)[i+1])
  meeting_tf[[i]] <- dat3 %>%
    select(word, temp) 
  tfs <- meeting_tf[[i]][[2]]
  names(tfs) <- meeting_tf[[i]][[1]]
  
  dat <- tail(sort(tfs),20)
  dat <- data.frame(word = names(dat), vals = dat)
  meeting_tf_plots[[i]] <- ggplot(dat, aes(x = reorder(word, -vals), y = vals)) +
    geom_col(fill = fills[i]) 
}

saveRDS(meeting_tf_plots, "tf_idf_plots.rds")
