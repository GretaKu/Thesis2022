rm(list = ls())

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/clean data")
library(haven)
library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(readxl)
library(rio)
library(twitteR)
library(rtweet)
library(academictwitteR)
library(swissparl)
library(cld2)
library(cld3)
library(wordcloud)
library(gmodels)

# Ideology ----------------------------------------------------------------

load("ideology.RData")

#recode ideology from smartvote (0-100 gradient Agreenesslevel to 0-10 categorical)
ideology <- x %>% mutate(
  cleavage_1_10 = ifelse(cleavage_1 > 0, round(cleavage_1 / 10, digits = 0), NA),
  cleavage_2_10 = ifelse(cleavage_2 > 0, round(cleavage_2 / 10, digits = 0), NA),
  cleavage_3_10 = ifelse(cleavage_3 > 0, round(cleavage_3 / 10, digits = 0), NA),
  cleavage_4_10 = ifelse(cleavage_4 > 0, round(cleavage_4 / 10, digits = 0), NA),
  cleavage_5_10 = ifelse(cleavage_5 > 0, round(cleavage_5 / 10, digits = 0), NA),
  cleavage_6_10 = ifelse(cleavage_6 > 0, round(cleavage_6 / 10, digits = 0), NA),
  cleavage_7_10 = ifelse(cleavage_7 > 0, round(cleavage_7 / 10, digits = 0), NA),
  cleavage_8_10 = ifelse(cleavage_8 > 0, round(cleavage_8 / 10, digits = 0), NA)
  
)

#creating unique ID of sample for all rows
ideology <- ideology %>% rename(ID_smartvote = ID_Candidate, ID_selects = ID) %>% 
  mutate(ID_sample = row_number())

n_distinct(ideology$ID_sample)

#merging Twitter ID to sample which was scraped in collection of tweet. -> for merge of new ideology dataset
load("twitter.RData")
twitter_key <- x %>% select(twitter_id, screen_name) %>% group_by(twitter_id, screen_name) %>% summarise()
ideology <- left_join(ideology, twitter_key, by = "screen_name")

export(ideology, "ideology2.RData")


# Ideology Distribution ---------------------------------------------------

# Creating frequency tables and labelling of the varibles
ideology_count <- data.frame(frq(ideology$ideology_scale))
ideology_count$variable <- "Ideology Scale (SELECTS)"
cleavage_1_count <- data.frame(frq(ideology$cleavage_1_10))
cleavage_2_count <- data.frame(frq(ideology$cleavage_2_10))
cleavage_3_count <- data.frame(frq(ideology$cleavage_3_10))
cleavage_4_count <- data.frame(frq(ideology$cleavage_4_10))
cleavage_5_count <- data.frame(frq(ideology$cleavage_5_10))
cleavage_6_count <- data.frame(frq(ideology$cleavage_6_10))
cleavage_7_count <- data.frame(frq(ideology$cleavage_7_10))
cleavage_8_count <- data.frame(frq(ideology$cleavage_8_10))
cleavage_1_count$variable <- "Cleavage 1: Open foreign policy (n=3908)"
cleavage_2_count$variable <- "Cleavage 2: Liberal economic policy (n=3919)"
cleavage_3_count$variable <- "Cleavage 3: Restrictive fiscal policy (n=3919)"
cleavage_4_count$variable <- "Cleavage 4: Law & order (n=3844)"
cleavage_5_count$variable <- "Cleavage 5: Restrictive migration policy (n=2968)"
cleavage_6_count$variable <- "Cleavage 6: Environmental protection (n=3915)"
cleavage_7_count$variable <- "Cleavage 7: Welfare state cleavage (n=3919)"
cleavage_8_count$variable <- "Cleavage 8: Liberal society (n=2130)"

# creating a dataframe including all frequencies
count_perCat <- rbind(ideology_count, cleavage_1_count, cleavage_2_count,
                             cleavage_3_count, cleavage_4_count, cleavage_5_count,
                             cleavage_6_count, cleavage_7_count, cleavage_8_count)


# Removing NA, rounding percentage, and selecting only frequencies
count_perCat <- count_perCat %>% filter(val >= 0 & val <= 10) %>% 
  mutate(raw.prc = round(as.numeric(raw.prc), 1)) %>% 
  select(variable, val, frq)

# Summarising to get total (100%) and merge total to frequencies
count_perCat_sum <- count_perCat %>% group_by(variable) %>% summarise(total = sum(frq))
count_perCat <- merge(count_perCat, count_perCat_sum, by = "variable")
# Calculation and rounding percentage 
count_perCat <- count_perCat %>% mutate(perc = round(frq/total*100, 1))

# Plotting Tweets distribution over scale
ggplot(data = count_perCat) +
  geom_col(mapping = aes(x = factor(val), y = perc)) +
  geom_text(aes(x = factor(val), y = perc, label = paste0(perc, "%")), size = 3, vjust = -0.3)+
  facet_wrap(~variable) +
  labs(x = "Ideological self-positioning", y = "Frequency in %") +
  labs(title = "Distribution of ideological self-positioning") +
  ylim(c(0,30)) +
  theme_bw()

ggsave("Ideology_Distribution.png", width = 9.7, height = 8)

# Tweets ------------------------------------------------------------------

load("tweets.RData")
tweets <- x %>% rename(twitter_id = author_id)

#Merging ideology information to tweets
tweets <- left_join(tweets, ideology, by = "twitter_id")
head(tweets)

#Only keep german
frq(tweets$language)
tweets_de <- tweets %>% filter(language == "de_CH")

# Authors per Ideologycat -------------------------------------------------
# Creating a dataframe with author count per Tweet

#selects
tweets <- tweets_de %>% group_by(twitter_id, ideology_scale) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_Selects <- tweets %>% group_by(ideology_scale) %>% summarise(n = n()) %>% 
  filter(!is.na(ideology_scale))#262 SELECTS

tweetsPerAuthor_Selects %>% ggplot() +
  geom_col(aes(x = factor(ideology_scale), y = n)) +
  geom_text(aes(x = factor(ideology_scale), y = n, label = n), size = 3.5, vjust = -0.3)+
  theme_bw() +
  labs(x = "Ideological self-positioning (SELECTS)", y = "Number of Authors (Twitter)",
       title = "Number of Tweet author by ideological self-positioning (n=262)") +
  ylim(c(0,70))
ggsave("TweetAuthos_SELECTS.png", width = 8.28, height = 4.7)

#smartvote
tweets <- tweets_de %>% group_by(twitter_id, cleavage_1_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav1 <- tweets %>% group_by(cleavage_1_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 1: Open foreign policy") %>% rename(val = cleavage_1_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_2_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav2 <- tweets %>% group_by(cleavage_2_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 2: Liberal economic policy") %>% rename(val = cleavage_2_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_3_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav3 <- tweets %>% group_by(cleavage_3_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 3: Restrictive fiscal policy") %>% rename(val = cleavage_3_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_4_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav4 <- tweets %>% group_by(cleavage_4_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 4: Law & order") %>% rename(val = cleavage_4_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_5_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav5 <- tweets %>% group_by(cleavage_5_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 5: Restrictive migration policy") %>% rename(val = cleavage_5_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_6_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav6 <- tweets %>% group_by(cleavage_6_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 6: Environmental protection") %>% rename(val = cleavage_6_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_7_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav7 <- tweets %>% group_by(cleavage_7_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 7: Welfare state cleavage") %>% rename(val = cleavage_7_10)#483 

tweets <- tweets_de %>% group_by(twitter_id, cleavage_8_10) %>% summarise(n = n()) #483 Users
tweetsPerAuthor_cleav8 <- tweets %>% group_by(cleavage_8_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 8: Liberal society") %>% rename(val = cleavage_8_10)#483 

# Merging all frequencies of cleavages to one large dataframe
tweetsPerAuthor_smartvote <- rbind(tweetsPerAuthor_cleav1, tweetsPerAuthor_cleav2, tweetsPerAuthor_cleav3, tweetsPerAuthor_cleav4,
      tweetsPerAuthor_cleav5, tweetsPerAuthor_cleav6, tweetsPerAuthor_cleav7, tweetsPerAuthor_cleav8)

#remove NA
tweetsPerAuthor_smartvote <- tweetsPerAuthor_smartvote %>% filter(!is.na(val))

tweetsPerAuthor_smartvote %>% ggplot() +
  geom_col(aes(x = factor(val), y = n)) +
  geom_text(aes(x = factor(val), y = n, label = n), size = 3.5, vjust = -0.3)+
  facet_wrap(~var) +
  theme_bw() +
  labs(x = "Ideological self-positioning (smartvote)", y = "Number of Authors (Twitter)",
       title = "Number of Tweet author by ideological self-positioning (n=483)")
ggsave("TweetAuthos_smartvote.png", width = 9.35, height = 7.52)

#Select only relevant variables
tweets_de2 <- tweets_de %>% 
  select(ID_sample, twitter_id, ID_smartvote, ID_selects, id, text, gender, age, ideology_scale,
    cleavage_1, cleavage_2, cleavage_3, cleavage_4, cleavage_5, cleavage_6, cleavage_7, cleavage_8,
    cleavage_1_10, cleavage_2_10, cleavage_3_10, cleavage_4_10, cleavage_5_10, cleavage_6_10, cleavage_7_10, cleavage_8_10) 


# Tweets Descriptivs ------------------------------------------------------

# Cleaning Tweets
url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
tweets_de2$text <- tolower(tweets_de2$text)
tweets_de2$text <- str_remove_all(tweets_de2$text, url_regex) #remove url
tweets_de2$text <- gsub("&amp", "", tweets_de2$text) #remove html entity
tweets_de2$text <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", tweets_de2$text) #remove rt via
tweets_de2$text <- gsub("@\\w+", "", tweets_de2$text) #remove mentions
tweets_de2$text <- str_replace_all(tweets_de2$text,"#[a-z,A-Z]*","") #remove hashtags
tweets_de2$text <- gsub("[^[:alnum:]///' ]", " ", tweets_de2$text)     #keep only alpha numeric

#Remove empty rows (Several entries had multiple spaces so this is to remove these)
tweets_de2 <- tweets_de2 %>% filter(text != "")
tweets_de2 <- tweets_de2 %>% filter(text != " ")
tweets_de2 <- tweets_de2 %>% filter(text != "  ")
tweets_de2 <- tweets_de2 %>% filter(text != "   ")
tweets_de2 <- tweets_de2 %>% filter(text != "    ")
tweets_de2 <- tweets_de2 %>% filter(text != "     ")
tweets_de2 <- tweets_de2 %>% filter(text != "      ")
tweets_de2 <- tweets_de2 %>% filter(text != "       ")

#export
export(tweets_de2, "tweets_ideology_de.RData")

# Length of Tweets in amount of characters
tweets_de2 <- tweets_de2 %>% mutate(text_length = str_count(text))
zeichen_tweets <- summary(tweets_de2$text_length)

# Age and Gender
descr_tweets <- tweets_de2 %>% select(gender, age) #for calculation differences significance
frq(tweets_de2$gender)
summary(tweets_de2$age)


# Wordfrequencies ---------------------------------------------------------

#Create Corpus to further clean data & Create relevant variables
tweets_de2 <- tweets_de2 %>% mutate(age_cat = ifelse(age <= 39, "20-39 years",
                                       ifelse(age >= 40 & age <= 59, "40-59 years",
                                              ifelse(age >= 60, "60+ years", NA))),
                                    gender = recode(gender, "f" = "female", "m"="male"))
frq(tweets_de2$age_cat)
twitter_selects <- tweets_de2 %>% select(text, gender, age_cat)
twitter_selects <- corpus(twitter_selects)

# Further cleaning procedure
twitter_selects2 <- tokens(twitter_selects , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                           split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
twitter_selects2 <- tokens_remove(twitter_selects2, c(stopwords("en"), stopwords("de"), "dass"))
twitter_selects2 <- tokens_remove(twitter_selects2, c("0*"))
twitter_selects2 <- tokens_wordstem(twitter_selects2, language = "german")


twitter_selects2 <- dfm(twitter_selects2)
topfeatures(twitter_selects2 , 100)  # 100 top words

textplot_wordcloud(twitter_selects2, min_count = 800, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

# calculate frequency by AGE GROUP
tstat2 <- textstat_frequency(twitter_selects2, n = 20, groups = "age_cat")

# plot frequencies
ggplot(data = tstat2, aes(x = factor(nrow(tstat2):1), y = frequency)) +
  geom_col() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(tstat2):1,
                   labels = tstat2$feature) +
  labs(x = NULL, y = NULL, title = "Frequency of word used by age group (N20-39=42789, 40-59=48546, 60+5682)") +
  theme_bw()+
  theme(axis.text=element_text(size=11))

ggsave("WordFreq_Twitter_age.png", width = 9.2, height = 5)


# calculate frequency by GENER
tstat2 <- textstat_frequency(twitter_selects2, n = 20, groups = "gender")

# plot frequencies
ggplot(data = tstat2, aes(x = factor(nrow(tstat2):1), y = frequency)) +
  geom_col() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(tstat2):1,
                   labels = tstat2$feature) +
  labs(x = NULL, y = NULL, title = "Frequency of word used by gender (F=35177, m = 61840 ") +
  theme_bw()+
  theme(axis.text=element_text(size=11))

ggsave("WordFreq_Twitter_gender.png", width = 9.2, height = 5)

# Transcript --------------------------------------------------------------

load("transcripts2.RData")
transcripts <- x %>% 
  rename(firstname = SpeakerFirstName, lastname = SpeakerLastName) %>% 
  filter(!is.na(firstname) | !is.na(lastname))

#Merging ideology information to transcripts
transcripts2 <- left_join(transcripts, ideology, by = c("firstname", "lastname"))
transcripts2 <- transcripts2 %>% filter(!is.na(ID_sample))


# Author descriptives -----------------------------------------------------

ID_Authors <- transcripts2 %>% group_by(ID_sample, ideology_scale,
                                        cleavage_1_10, cleavage_2_10, cleavage_3_10, cleavage_4_10, 
                                        cleavage_5_10, cleavage_6_10, cleavage_7_10, cleavage_8_10) %>% summarise(n = n())

trans_selects <- ID_Authors %>% group_by(ideology_scale) %>% summarise(n = n()) %>% 
  mutate(var = "Ideological scale (SELECTS)") %>% rename(scale = ideology_scale)
trans_cleav1 <- ID_Authors %>% group_by(cleavage_1_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 1: Open foreign policy") %>% rename(scale = cleavage_1_10)
trans_cleav2 <- ID_Authors %>% group_by(cleavage_2_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 2: Liberal economic policy") %>% rename(scale = cleavage_2_10)
trans_cleav3 <- ID_Authors %>% group_by(cleavage_3_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 3: Restrictive fiscal policy") %>% rename(scale = cleavage_3_10)
trans_cleav4 <- ID_Authors %>% group_by(cleavage_4_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 4: Law & order") %>% rename(scale = cleavage_4_10)
trans_cleav5 <- ID_Authors %>% group_by(cleavage_5_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 5: Restrictive migration policy") %>% rename(scale = cleavage_5_10)
trans_cleav6 <- ID_Authors %>% group_by(cleavage_6_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 6: Environmental protection") %>% rename(scale = cleavage_6_10)
trans_cleav7 <- ID_Authors %>% group_by(cleavage_7_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 7: Welfare state cleavage") %>% rename(scale = cleavage_7_10)
trans_cleav8 <- ID_Authors %>% group_by(cleavage_8_10) %>% summarise(n = n()) %>% 
  mutate(var = "Cleavage 8: Liberal society") %>% rename(scale = cleavage_8_10)

trans_ideology <- rbind(trans_selects, trans_cleav1,trans_cleav2, trans_cleav3, trans_cleav4, trans_cleav5,
                        trans_cleav6, trans_cleav7, trans_cleav8)

trans_ideology <- trans_ideology %>% filter(!is.na(scale)) 

trans_ideology_sum <- trans_ideology %>% group_by(var) %>% summarise(total = sum(n))
trans_ideology <- merge(trans_ideology, trans_ideology_sum, by = "var")
trans_ideology <- trans_ideology %>% mutate(perc = round(n/total*100, 1))

trans_selects %>% filter(!is.na(scale)) %>% ggplot() +
  geom_col(aes(x = factor(scale), y = n)) +
  geom_text(aes(x = factor(scale), y = n, label = n), size = 3.5, vjust = -0.3)+
  theme_bw() +
  labs(x = "Ideological self-positioning (SELECTS)", y = "Number of candidates with transcripts",
       title = "Number of candidates with transcripts by ideological self-positioning")
ggsave("CandidatesTranscripts_SELECTS.png", width = 8.28, height = 4.7)



# Descriptives Transcript -------------------------------------------------
#Select only relevant variables
transcripts2 <- transcripts2 %>% 
  select(ID_sample, IdSession, ID_smartvote, ID_selects, Text, Text2, gender, age, ideology_scale,
         cleavage_1, cleavage_2, cleavage_3, cleavage_4, cleavage_5, cleavage_6, cleavage_7, cleavage_8,
         cleavage_1_10, cleavage_2_10, cleavage_3_10, cleavage_4_10, cleavage_5_10, cleavage_6_10, cleavage_7_10, cleavage_8_10) %>% 
  
  mutate(text_length = str_count(Text2)) #Determine length of text

summary(transcripts2$text_length)

## Creating more rows by splitting strings
transcripts3 <- transcripts2 %>% separate_rows(Text, sep = "</p>") %>% 
  mutate(Text3 = clean_text(Text),
         text_length = str_count(Text3)) %>% 
  filter(!is.na(Text3)) %>% 
  filter(Text3 != "")

summary(transcripts3$text_length)#1 - 261 Words Range

frq(transcripts3$ideology_scale)

#export
export(transcripts3, "transcripts_ideology_de.RData")
export(transcripts3, "transcripts_ideology_de.xlsx")

load("transcripts_ideology_de.RData")
x <- x %>% filter(Text3 != "")

# Descriptive facts:
summary(x$age)
frq(x$gender)

#Calculating Significance in difference of age and gender
descr_tweets$sample <- "Tweets"
descr_transc <- x %>% select(gender, age) %>% mutate(sample = "Transc")
descr <- rbind(descr_transc, descr_tweets)

# T-Test for age difference
t.test(descr$age~descr$sample)

# Wordfrequencies ---------------------------------------------------------

#Create Corpus to further clean data & Create relevant variables
transcript <- x %>% mutate(age_cat = ifelse(age <= 39, "20-39 years",
                                                     ifelse(age >= 40 & age <= 59, "40-59 years",
                                                            ifelse(age >= 60, "60+ years", NA))),
                           gender = recode(gender, "f" = "female", "m"="male"))
frq(transcript$age_cat)
frq(transcript$gender)
transcript <- transcript %>% select(Text3, gender, age_cat) %>% rename(text = Text3)
transcript_corp <- corpus(transcript)

# Further cleaning procedure
transcript_corp <- tokens(transcript_corp , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                           split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
transcript_corp <- tokens_remove(transcript_corp, c(stopwords("en"), stopwords("de"), "dass"))
transcript_corp <- tokens_remove(transcript_corp, c("0*"))
transcript_corp <- tokens_wordstem(transcript_corp, language = "german")


transcript_DFM <- dfm(transcript_corp)
topfeatures(transcript_DFM , 100)  # 100 top words

textplot_wordcloud(transcript_DFM, min_count = 500, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

# calculate frequency by AGE GROUP
tstat2 <- textstat_frequency(transcript_DFM, n = 20, groups = "age_cat")

# plot frequencies
ggplot(data = tstat2, aes(x = factor(nrow(tstat2):1), y = frequency)) +
  geom_col() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(tstat2):1,
                   labels = tstat2$feature) +
  labs(x = NULL, y = NULL, title = "Frequency of word used by age group (N20-39=995, 40-59=8682, 60+3304)") +
  theme_bw() +
  theme(axis.text=element_text(size=11))

ggsave("WordFreq_Transc_age.png", width = 9.2, height = 5)


# calculate frequency by GENER
tstat2 <- textstat_frequency(transcript_DFM, n = 20, groups = "gender")

# plot frequencies
ggplot(data = tstat2, aes(x = factor(nrow(tstat2):1), y = frequency)) +
  geom_col() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_discrete(breaks = nrow(tstat2):1,
                   labels = tstat2$feature) +
  labs(x = NULL, y = NULL, title = "Frequency of word used by gender (F=4697, m = 8284 ") +
  theme_bw()+
  theme(axis.text=element_text(size=11))

ggsave("WordFreq_Transc_gender.png", width = 9.2, height = 5)




# EXPORT individual files -------------------------------------------------
# TWITTER -----------------------------------------------------------------
rm(list = ls())

load("tweets_ideology_de.RData")
twitter <- x

# Twitter - Selects 
twitter_selects <- twitter %>% 
  mutate(ideology_scale = ifelse(ideology_scale >= 0 & ideology_scale <= 10, ideology_scale, NA)) %>% 
  select(text, ideology_scale) %>% 
  drop_na()
export(twitter_selects, "twitter_selects.RData")


# Twitter - cleavage_1 
twitter_cleavage_1 <- twitter %>% 
  mutate(cleavage_1 = ifelse(cleavage_1 >= 0 & cleavage_1 <= 100, cleavage_1, NA),
         cleavage_1_10 = ifelse(cleavage_1_10 >= 0 & cleavage_1_10 <= 10, cleavage_1_10, NA)) %>% 
  select(text, cleavage_1, cleavage_1_10) %>% 
  drop_na()
export(twitter_cleavage_1, "twitter_cleavage_1.RData")

# Twitter - cleavage_2 
twitter_cleavage_2 <- twitter %>% 
  mutate(cleavage_2 = ifelse(cleavage_2 >= 0 & cleavage_2 <= 100, cleavage_2, NA),
         cleavage_2_10 = ifelse(cleavage_2_10 >= 0 & cleavage_2_10 <= 10, cleavage_2_10, NA)) %>% 
  select(text, cleavage_2, cleavage_2_10) %>% 
  drop_na()
export(twitter_cleavage_2, "twitter_cleavage_2.RData")

# Twitter - cleavage_3 
twitter_cleavage_3 <- twitter %>% 
  mutate(cleavage_3 = ifelse(cleavage_3 >= 0 & cleavage_3 <= 100, cleavage_3, NA),
         cleavage_3_10 = ifelse(cleavage_3_10 >= 0 & cleavage_3_10 <= 10, cleavage_3_10, NA)) %>% 
  select(text, cleavage_3, cleavage_3_10) %>% 
  drop_na()
export(twitter_cleavage_3, "twitter_cleavage_3.RData")

# Twitter - cleavage_4 
twitter_cleavage_4 <- twitter %>% 
  mutate(cleavage_4 = ifelse(cleavage_4 >= 0 & cleavage_4 <= 100, cleavage_4, NA),
         cleavage_4_10 = ifelse(cleavage_4_10 >= 0 & cleavage_4_10 <= 10, cleavage_4_10, NA)) %>% 
  select(text, cleavage_4, cleavage_4_10) %>% 
  drop_na()
export(twitter_cleavage_4, "twitter_cleavage_4.RData")

# Twitter - cleavage_5 
twitter_cleavage_5 <- twitter %>% 
  mutate(cleavage_5 = ifelse(cleavage_5 >= 0 & cleavage_5 <= 100, cleavage_5, NA),
         cleavage_5_10 = ifelse(cleavage_5_10 >= 0 & cleavage_5_10 <= 10, cleavage_5_10, NA)) %>% 
  select(text, cleavage_5, cleavage_5_10) %>% 
  drop_na()
export(twitter_cleavage_5, "twitter_cleavage_5.RData")


# Twitter - cleavage_6 
twitter_cleavage_6 <- twitter %>% 
  mutate(cleavage_6 = ifelse(cleavage_6 >= 0 & cleavage_6 <= 100, cleavage_6, NA),
         cleavage_6_10 = ifelse(cleavage_6_10 >= 0 & cleavage_6_10 <= 10, cleavage_6_10, NA)) %>% 
  select(text, cleavage_6, cleavage_6_10) %>% 
  drop_na()
export(twitter_cleavage_6, "twitter_cleavage_6.RData")

# Twitter - cleavage_7 
twitter_cleavage_7 <- twitter %>% 
  mutate(cleavage_7 = ifelse(cleavage_7 >= 0 & cleavage_7 <= 100, cleavage_7, NA),
         cleavage_7_10 = ifelse(cleavage_7_10 >= 0 & cleavage_7_10 <= 10, cleavage_7_10, NA)) %>% 
  select(text, cleavage_7, cleavage_7_10) %>% 
  drop_na()
export(twitter_cleavage_7, "twitter_cleavage_7.RData")

# Twitter - cleavage_8 
twitter_cleavage_8 <- twitter %>% 
  mutate(cleavage_8 = ifelse(cleavage_8 >= 0 & cleavage_8 <= 100, cleavage_8, NA),
         cleavage_8_10 = ifelse(cleavage_8_10 >= 0 & cleavage_8_10 <= 10, cleavage_8_10, NA)) %>% 
  select(text, cleavage_8, cleavage_8_10) %>% 
  drop_na()
export(twitter_cleavage_8, "twitter_cleavage_8.RData")


# TRANSCRIPTS -------------------------------------------------------------
rm(list = ls())
load("transcripts_ideology_de.RData")
transcript <- x

# Transcript - Selects 
transcript_selects <- transcript %>% 
  mutate(ideology_scale = ifelse(ideology_scale >= 0 & ideology_scale <= 10, ideology_scale, NA)) %>% 
  select(Text3, ideology_scale) %>% 
  drop_na()
export(transcript_selects, "transcript_selects.RData")


# Transcript - cleavage_1 
transcript_cleavage_1 <- transcript %>% 
  mutate(cleavage_1 = ifelse(cleavage_1 >= 0 & cleavage_1 <= 100, cleavage_1, NA),
         cleavage_1_10 = ifelse(cleavage_1_10 >= 0 & cleavage_1_10 <= 10, cleavage_1_10, NA)) %>% 
  select(Text3, cleavage_1, cleavage_1_10) %>% 
  drop_na()
export(transcript_cleavage_1, "transcript_cleavage_1.RData")

# Transcript - cleavage_2 
transcript_cleavage_2 <- transcript %>% 
  mutate(cleavage_2 = ifelse(cleavage_2 >= 0 & cleavage_2 <= 100, cleavage_2, NA),
         cleavage_2_10 = ifelse(cleavage_2_10 >= 0 & cleavage_2_10 <= 10, cleavage_2_10, NA)) %>% 
  select(Text3, cleavage_2, cleavage_2_10) %>% 
  drop_na()
export(transcript_cleavage_2, "transcript_cleavage_2.RData")

# Transcript - cleavage_3 
transcript_cleavage_3 <- transcript %>% 
  mutate(cleavage_3 = ifelse(cleavage_3 >= 0 & cleavage_3 <= 100, cleavage_3, NA),
         cleavage_3_10 = ifelse(cleavage_3_10 >= 0 & cleavage_3_10 <= 10, cleavage_3_10, NA)) %>% 
  select(Text3, cleavage_3, cleavage_3_10) %>% 
  drop_na()
export(transcript_cleavage_3, "transcript_cleavage_3.RData")

# Transcript - cleavage_4 
transcript_cleavage_4 <- transcript %>% 
  mutate(cleavage_4 = ifelse(cleavage_4 >= 0 & cleavage_4 <= 100, cleavage_4, NA),
         cleavage_4_10 = ifelse(cleavage_4_10 >= 0 & cleavage_4_10 <= 10, cleavage_4_10, NA)) %>% 
  select(Text3, cleavage_4, cleavage_4_10) %>% 
  drop_na()
export(transcript_cleavage_4, "transcript_cleavage_4.RData")

# Transcript - cleavage_5 
transcript_cleavage_5 <- transcript %>% 
  mutate(cleavage_5 = ifelse(cleavage_5 >= 0 & cleavage_5 <= 100, cleavage_5, NA),
         cleavage_5_10 = ifelse(cleavage_5_10 >= 0 & cleavage_5_10 <= 10, cleavage_5_10, NA)) %>% 
  select(Text3, cleavage_5, cleavage_5_10) %>% 
  drop_na()
export(transcript_cleavage_5, "transcript_cleavage_5.RData")


# Transcript - cleavage_6 
transcript_cleavage_6 <- transcript %>% 
  mutate(cleavage_6 = ifelse(cleavage_6 >= 0 & cleavage_6 <= 100, cleavage_6, NA),
         cleavage_6_10 = ifelse(cleavage_6_10 >= 0 & cleavage_6_10 <= 10, cleavage_6_10, NA)) %>% 
  select(Text3, cleavage_6, cleavage_6_10) %>% 
  drop_na()
export(transcript_cleavage_6, "transcript_cleavage_6.RData")

# Transcript - cleavage_7 
transcript_cleavage_7 <- transcript %>% 
  mutate(cleavage_7 = ifelse(cleavage_7 >= 0 & cleavage_7 <= 100, cleavage_7, NA),
         cleavage_7_10 = ifelse(cleavage_7_10 >= 0 & cleavage_7_10 <= 10, cleavage_7_10, NA)) %>% 
  select(Text3, cleavage_7, cleavage_7_10) %>% 
  drop_na()
export(transcript_cleavage_7, "transcript_cleavage_7.RData")

# Transcript - cleavage_8 
transcript_cleavage_8 <- transcript %>% 
  mutate(cleavage_8 = ifelse(cleavage_8 >= 0 & cleavage_8 <= 100, cleavage_8, NA),
         cleavage_8_10 = ifelse(cleavage_8_10 >= 0 & cleavage_8_10 <= 10, cleavage_8_10, NA)) %>% 
  select(Text3, cleavage_8, cleavage_8_10) %>% 
  drop_na()
export(transcript_cleavage_8, "transcript_cleavage_8.RData")


