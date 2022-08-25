rm(list = ls())

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/left right meaning")

library(tidyverse)
library(rio)
library(tidytext)
library(dplyr)

meaning_left <- tibble(import("meaning_left.RData"))
meaning_right <- tibble(import("meaning_right.RData"))

#stopwords
stop_german <- data.frame(word = c(stopwords::stopwords("de"), letters, ), stringsAsFactors = FALSE)

#Frequencies
freq_left <- meaning_left %>% 
  unnest_tokens(word, left_means) %>% 
  anti_join(stop_german) %>% 
  count(word, sort = TRUE) 

freq_right <- meaning_right %>% 
  unnest_tokens(word, right_means) %>% 
  anti_join(stop_german) %>% 
  count(word, sort = TRUE)


(p1 <- freq_left %>% 
    top_n(20) %>%  
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n))+
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(y = "word count", x = "", title = "Top 20 words for describing left (N=4014)") +
  geom_text(aes(word, n, geom = "text", label = paste0("n=",n)), color = "white", hjust = 1.1) +
  theme_bw())

ggsave("WordcountLeft.png", p1, height = 4, width = 9)

(p1 <- freq_right %>% 
    top_n(20) %>%  
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n))+
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(y = "word count", x = "", title = "Top 20 words for describing right (N=3705)") +
    geom_text(aes(word, n, geom = "text", label = paste0("n=",n)), color = "white", hjust = 1.1) +
    theme_bw())

ggsave("WordcountRight.png", p1, height = 4, width = 9)

# Sentiment Analysis ------------------------------------------------------
meaning_left <- meaning_left %>% 
  unnest_tokens(word, left_means) %>% 
  anti_join(stop_german)

meaning_right <- meaning_right %>%
  unnest_tokens(word, right_means) %>% 
  anti_join(stop_german)

# Word Dictionary prep
#https://wortschatz.uni-leipzig.de/de/download
neg_df <- read_tsv("SentiWS_v2.0_Negative.txt", col_names = FALSE)
names(neg_df) <- c("Wort_POS", "Wert", "Inflektionen")

neg_df <- neg_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1))


pos_df <- read_tsv("SentiWS_v2.0_Positive.txt", col_names = FALSE)
names(pos_df) <- c("Wort_POS", "Wert", "Inflektionen")

pos_df <- pos_df %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) 

neg <- neg_df %>% select(Wert, Wort) %>% rename(value = Wert, word = Wort) 
pos <- pos_df %>% select(Wert, Wort) %>% rename(value = Wert, word = Wort)

sentiments <- rbind(neg, pos)
sentiments$word <- tolower(sentiments$word)


# Merging Sentiments to Text
meaning_all <- bind_rows(mutate(meaning_left, case = "left"),
                         mutate(meaning_right, case = "right")) 


sa <- left_join(meaning_all, sentiments, by = "word")

sa2 <- sa %>% mutate(
  sentiment = ifelse(value > 0, "positive",
                     ifelse(value < 0, "negative", NA)))


# Word Count SentimentAnalysis
sa_word_count <- sa2 %>% 
  count(word, sentiment, case, sort = TRUE) %>% 
  filter(!is.na(sentiment))


g <- sa_word_count %>% 
  filter(sentiment == "positive") %>% 
  top_n(20) %>%  
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n))+
  geom_col(show.legend = FALSE, fill = "darkgreen") +
  facet_wrap(~case, scales = "free_y")+
  coord_flip() +
  labs(y = "word count", x = "", title = "Top 20 words with positive sentiment") +
  theme_bw() +
  geom_text(aes(word, n, geom = "text", label = paste0("n=",n)), color = "white", hjust = 1.1)

ggsave("SA_wordcount_positive.png", g, height = 4, width = 9)


(n1 <- sa_word_count %>% 
    filter(sentiment == "negative") %>% 
    top_n(20) %>%  
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n))+
    geom_col(show.legend = FALSE, fill = "darkred") +
    facet_wrap(~case, scales = "free_y")+
    coord_flip() +
    labs(y = "word count", x = "", title = "Top 20 words with negative sentiment")+
    theme_bw()+
    geom_text(aes(word, n, geom = "text", label = paste0("n=",n)), color = "white", hjust = 1.1))

ggsave("SA_wordcount_negative.png", n1, height = 4, width = 9)
  







