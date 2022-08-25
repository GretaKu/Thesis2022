rm(list = ls())

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data")
library(haven)
library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(readxl)
library(rio)
library(twitteR)
library(rtweet)
library(academictwitteR)

# Ideology: SELECT --------------------------------------------------------

#Import Candidate Survey 
selects_19 <- read_dta(here::here("rawdata", "1186_Selects2019_CandidateSurvey_Data_v1.1.0.dta"))

#selecting relevant columns
selects <- selects_19 %>% 
  select(T2, C5a, E14a, E14b, T11a) %>% 
  rename(ID = T2, ideology = C5a, left_means = E14a, right_means = E14b, elected = T11a)

#Import key so personal information can be merged to survey
key <- read_spss(here::here("rawdata","2019_Key_CandidateSurvey.sav"))
key <- key %>% rename(ID = T2, firstname = T2g, lastname = T2h, party = T9b)

selects <- left_join(selects, key, by = "ID")


# Ideology: SmartVote -----------------------------------------------------

smartvote_19 <- read_excel(here::here("rawdata","candidates_2019_nrsr_all_cleavages_links_korr.xlsx"), sheet = "NR")

smartvote <- smartvote_19 %>% 
  select(ID_Candidate, firstname, lastname, gender, age, language, LINK_twitter, cleavage_1, cleavage_2, cleavage_3,
         cleavage_4, cleavage_5, cleavage_6, cleavage_7, cleavage_8) 

#extracting key to merge key to selects
smartvote_key <- smartvote %>% select(ID_Candidate, firstname, lastname)


# Merging to Ideology dataset ---------------------------------------------

#first: merging smartvote key to selects
selects_wKey <- merge(selects, smartvote_key, by = c("firstname", "lastname"), all = T)
#export(selects_wKey, "checkNA_selects_wKey.xlsx")
#checked for SPELLING ERROR in name-> No typos found

#second: merging ideology datasets by key
ideology <- merge(smartvote, selects_wKey, by = "ID_Candidate", all = T)

ideology <- ideology %>% select(-firstname.y, -lastname.y) %>% 
  rename(firstname = firstname.x,
         lastname = lastname.x,
         ideology_scale = ideology)

#Create CLEAN string with screen names
ideology <- ideology  %>% 
  mutate(temp_location = as.numeric(str_sub(as.character(str_locate_all(LINK_twitter, "\\?")), 3, 4)), #retrieving location of ? in string
         
         screen_name = ifelse(grepl("?", LINK_twitter, fixed = T), str_sub(LINK_twitter, start = 1, end = temp_location-1), LINK_twitter), #cutting string before ? 
         
         screen_name = ifelse(str_detect(screen_name,"https://twitter.com/"), str_sub(screen_name, 21, 36),
                              ifelse(str_detect(screen_name,"https://mobile.twitter.com/"), str_sub(screen_name, 28, 43),
                                     ifelse(str_detect(screen_name,"http://www.twitter.com/"), str_sub(screen_name, 23, 38),
                                            ifelse(str_detect(screen_name,"https://www.twitter.com/"), str_sub(screen_name, 24, 39),
                                                   ifelse(str_detect(screen_name,"http://twitter.com/"), str_sub(screen_name, 20, 35), NA))))),
         
         screen_name = str_remove_all(screen_name, "[/]"),
         screen_name = str_remove_all(screen_name, "[@]"),
         screen_name = str_remove_all(screen_name, "[.]")) %>% 
  
  select(-temp_location)



export(ideology, "ideology.RData")
export(ideology, "ideology.xlsx")

## DESCRIPTIVES: TWITTER USER PER DATASET
ideology_descr <- ideology %>% mutate(twitter_binary = ifelse(!is.na(screen_name) & screen_name != "", 1,0),
                                      smartvote = ifelse(!is.na(ID_Candidate), 1, 0),
                                      selects = ifelse(!is.na(ID), 1, 0))

ideology_descr %>% 
  group_by(twitter_binary) %>% 
  summarise(n = n())



# Twitter Scrape ----------------------------------------------------------
#API
#set_bearer() #Enter bearer and restart 
get_bearer() #https://cran.r-project.org/web/packages/academictwitteR/vignettes/academictwitteR-intro.html

twitter <- ideology  %>% 
  filter(!is.na(screen_name) & screen_name != "")


bearer_token <- get_bearer()
#Making packages to retrieve ID (Error message when get_user_id used with more than 100 handles)

all_ids <- NULL

twi1 <- twitter[1:100,] ; hand1 <- twi1$screen_name 
id <- get_user_id(hand1, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi2 <- twitter[101:200,] ; hand <- twi2$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi3 <- twitter[201:300,]; hand <- twi3$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi4 <- twitter[301:400,] ; hand <- twi4$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi5 <- twitter[401:500,] ; hand <- twi5$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi6 <- twitter[501:600,] ; hand <- twi6$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi7 <- twitter[601:700,] ; hand <- twi7$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi8 <- twitter[701:800,] ; hand <- twi8$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)

twi9 <- twitter[801:806,] ; hand <- twi9$screen_name
id <- get_user_id(hand, bearer_token = bearer_token, all = TRUE)
all_ids <- rbind(all_ids, id)



#merging twitter IDs to dataset
names(all_ids)[3] <- "screen_name"
twitter <- merge(twitter, all_ids, by = "screen_name", all.x = T)
twitter <- twitter %>% filter(!is.na(id)) %>% rename(twitter_id = id)



## SCRAPER ##

ids <- twitter$twitter_id

tweets <- NULL
#using try() so if no tweets are scraped and error occures, programm keeps running
for (i in (1:length(ids))) {
  try(x1 <- get_user_timeline(ids[i], 
                          start_tweets = "2019-01-01T00:00:00Z", 
                          end_tweets = "2020-01-01T00:00:00Z",
                          bearer_token = get_bearer(),
                          n = 3200))
  
  try(x1 <- x1 %>% select(created_at, text, author_id, id, source, public_metrics))
  try(x1 <- as.data.frame(x1))
  
  try(tweets <- rbind(tweets, x1))
  
}

export(tweets, "tweets.RData")
export(tweets, "tweets.xlsx")

n_distinct(tweets$author_id)

## Merging tweets to twitter dataset with ideology
twitter <- merge(twitter, tweets, by.x = "twitter_id", by.y = "author_id")

export(twitter, "twitter.RData")
export(twitter, "twitter.xlsx")




# Transcript Scrape -------------------------------------------------------


# Transcripts scrape
  library(swissparl)

x <- get_glimpse(table = "Transcript", rows = 50, Language = "DE") 
get_variables(table = "Transcript")

# Selecting Session: Wintersession 2019 = 5101 / Herbstsession 2019 = 5019 / Sommersession 2019 = 5018 / 
# Sondersession Mai 2019 = 5017 / Frühjahrssession 2019 = 5016
transcripts <- get_data(table = "Transcript", IdSession = c("5101", "5019", "5018", "5017", "5016")) %>%
  mutate(Text2 = clean_text(Text))

#export(transcripts, "transcripts.RData")
#export(transcripts, "transcripts.xlsx")

load("transcripts.RData")
transcripts <- x

transcripts2 <- transcripts %>% 
  select(SpeakerFirstName, SpeakerLastName, Text, Text2, LanguageOfText, Language,
         IdSession, MeetingDate, CouncilId, CouncilName) %>% 
  filter(Language == "DE" & LanguageOfText == "DE")


export(transcripts2, "transcripts2.RData")
export(transcripts2, "transcripts2.xlsx")
