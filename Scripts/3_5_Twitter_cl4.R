rm(list = ls())

setwd("C:/Users/paperspace/Documents/Thesis")
library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(quanteda)
library(tm)
library(parallel)
library(parallelMap)
library(mlr)
library(rio)

# ML TWITTER --------------------------------------------------------------

load("twitter_cleavage_4.RData")

# Cleaning Tweets
url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
x$text <- tolower(x$text)
x$text <- str_remove_all(x$text, url_regex) #remove url
x$text <- gsub("&amp", "", x$text) #remove html entity
x$text <- gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x$text) #remove rt via
x$text <- gsub("@\\w+", "", x$text) #remove mentions
x$text <- str_replace_all(x$text,"#[a-z,A-Z]*","") #remove hashtags
x$text <- gsub("[^[:alnum:]///' ]", " ", x$text)     #keep only alpha numeric


# 2. Balance to reduce size (7-point-scale)  ---------------------------
x_7 <- x %>% mutate(
  ideology_scale7 = ifelse(cleavage_4_10 == 9 | cleavage_4_10 == 10, "7:Strong Agree",
                           ifelse(cleavage_4_10 == 7 | cleavage_4_10 == 8, "6:Agree",
                                  ifelse(cleavage_4_10 == 6, "5:Moderate Agree",
                                         ifelse(cleavage_4_10 == 5, "4:Neutral",
                                                ifelse(cleavage_4_10 == 4, "3:Moderate Disagree",
                                                       ifelse(cleavage_4_10 == 3 | cleavage_4_10 == 2, "2:Disagree",
                                                              ifelse(cleavage_4_10 == 1 | cleavage_4_10 == 0, "1:Strong Disagree", NA))))))),
  ideology_scale7 = factor(ideology_scale7))

frq(x_7$cleavage_4_10)
frq(x_7$ideology_scale7) # Manual Check if groups are correctly allocated






tweets_cleav4_doc_7 <- data.frame(frq(x_7$ideology_scale7))

twitter_cleav4 = NULL
y = NULL
vec = c("1:Strong Disagree", "2:Disagree", "3:Moderate Disagree", "4:Neutral", 
        "5:Moderate Agree", "6:Agree", "7:Strong Agree") 
for (i in vec) {
  y <- x_7 %>% 
    filter(ideology_scale7 == i) %>% 
    sample_n(947)
  
  twitter_cleav4 <- rbind(y, twitter_cleav4)
}

frq(twitter_cleav4$ideology_scale7)

#Removing extra scale to avoid error in coding
twitter_cleav4 <- twitter_cleav4 %>% select(-cleavage_4_10, -cleavage_4)

# Create DFM  -----------------------------------------------------------------
twitter_cleav4 <- corpus(twitter_cleav4)

# Further cleaning procedure
twitter_cleav42 <- tokens(twitter_cleav4 , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                          split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
twitter_cleav42 <- tokens_remove(twitter_cleav42, c(stopwords("en"), stopwords("de"), "dass", "مع", "ممتاز"))

twitter_cleav42 <- tokens_remove(twitter_cleav42, c("0*"))
twitter_cleav42 <- tokens_wordstem(twitter_cleav42, language = "german")
twitter_cleav42 <- dfm(twitter_cleav42)
topfeatures(twitter_cleav42 , 1000)  # Check Topfeatures and in how many docs they are

# Trimming 1: Remove features that appear in less than 95 Tweets
twitter_cleav42 <- dfm_trim(twitter_cleav42 , min_docfreq = 10, verbose=TRUE)
twitter_cleav42  <- dfm_remove(twitter_cleav42 , min_nchar = 2)
topfeatures(twitter_cleav42 , 20)  # 20 top words

# Trimming 2: Remove doc with only 0
twitter_cleav42[ntoken(twitter_cleav42) == 0,]
twitter_cleav42  <-  twitter_cleav42[ntoken(twitter_cleav42) != 0,]
twitter_cleav42[ntoken(twitter_cleav42) == 0,]



twitter_cleav4_df <- twitter_cleav42 %>% 
  quanteda::convert(to = "data.frame") %>% 
  cbind(docvars(twitter_cleav42)) %>% 
  select(-doc_id)


# solving problem of features starting with digits eg 2x
names(twitter_cleav4_df) <- make.names(names(twitter_cleav4_df))

frq(twitter_cleav4_df$ideology_scale7)
tweets_cleav4_fea_7 <- data.frame(frq(x$ideology_scale7))

# Task
ml_task <- makeClassifTask(data = twitter_cleav4_df, target = "ideology_scale7")

# kNN ---------------------------------------------------------------------
parallelStartSocket(cpus = detectCores())

#Hyper param Tuning - 
gridSearch <- makeTuneControlGrid()
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:5))

cv <- makeResampleDesc("CV", iters = 3, stratify = T)

tunedK <- tuneParams("classif.knn", task = ml_task,
                     resampling = cv,
                     par.set = knnParamSpace,
                     control = gridSearch)

tunedK
knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean",
                    plot.type = "line") +
  theme_bw()



#Learner
knn <- makeLearner("classif.knn", par.vals = list("k" = 1))

#Crossvalidation
cv_knn <- resample(learner = knn, task = ml_task,
                   resampling = cv, measures = list(mmce, acc))


#Performance
conf_matrix_knn <- calculateConfusionMatrix(cv_knn$pred, relative = FALSE)
conf_matrix_knn <- cbind(rownames(conf_matrix_knn$result), as_tibble(conf_matrix_knn$result))
export(conf_matrix_knn, file = "TwCl4_confmatrix_knn.xlsx")

knn_predictions <- getRRPredictionList(cv_knn)
knn_1_fold <- as.data.frame(knn_predictions$test$`1`)
knn_2_fold <- as.data.frame(knn_predictions$test$`2`)
knn_3_fold <- as.data.frame(knn_predictions$test$`3`)
 
export(knn_1_fold, "pred_TwCl4_knn1.xlsx")
export(knn_2_fold, "pred_TwCl4_knn2.xlsx")
export(knn_3_fold, "pred_TwCl4_knn3.xlsx")

# Naive Bays --------------------------------------------------------------

#Set Up Model
bayes <- makeLearner("classif.naiveBayes")


#CV Procedure
kFold <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)

bayesCV <- resample(learner = bayes, task = ml_task,
                    resampling = kFold, measures = list(mmce, acc))

#Performance
conf_matrix_NB <- calculateConfusionMatrix(bayesCV$pred, relative = FALSE)
conf_matrix_NB <- cbind(rownames(conf_matrix_NB$result), as_tibble(conf_matrix_NB$result))
export(conf_matrix_NB, file = "TwCl4_confmatrix_NB.xlsx")

nb_predictions <- getRRPredictionList(bayesCV)
nb_1_fold <- as.data.frame(nb_predictions$test$`1`)
nb_2_fold <- as.data.frame(nb_predictions$test$`2`)
nb_3_fold <- as.data.frame(nb_predictions$test$`3`)

export(nb_1_fold, "pred_TwCl4_nb1.xlsx")
export(nb_2_fold, "pred_TwCl4_nb2.xlsx")
export(nb_3_fold, "pred_TwCl4_nb3.xlsx")

# SVM ---------------------------------------------------------------------

svm <- makeLearner("classif.svm")

kernels <- c("polynomial", "radial","sigmoid")

svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = kernels),
  makeIntegerParam("degree", lower = 1, upper = 3),
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.1, 10))


## Defining Random Search & Tuning Parameters
randSearch <- makeTuneControlRandom(maxit = 20)
cvForTuning <- makeResampleDesc("Holdout", split = 2/3)


# CV Model Building Process
outer <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning,
                              par.set = svmParamSpace,
                              control = randSearch)


parallelStartSocket(cpus = detectCores())
cvWithTuning <- resample(svmWrapper, ml_task, resampling = outer)


#Performance
conf_matrix_SVM <- calculateConfusionMatrix(cvWithTuning$pred, relative = FALSE)
conf_matrix_SVM <- cbind(rownames(conf_matrix_SVM$result), as_tibble(conf_matrix_SVM$result))
export(conf_matrix_SVM, file = "TwCl4_confmatrix_SVM.xlsx")

svm_predictions <- getRRPredictionList(cvWithTuning)
svm_1_fold <- as.data.frame(svm_predictions$test$`1`)
svm_2_fold <- as.data.frame(svm_predictions$test$`2`)
svm_3_fold <- as.data.frame(svm_predictions$test$`3`)

export(svm_1_fold, "pred_TwCl4_svm1.xlsx")
export(svm_2_fold, "pred_TwCl4_svm2.xlsx")
export(svm_3_fold, "pred_TwCl4_svm3.xlsx")

# Random Forest -----------------------------------------------------------

forest <- makeLearner("classif.randomForest")

# Hyperparameter Tuning
forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 300, upper = 300),
  makeIntegerParam("mtry", lower = 10, upper = 100), #cause lots of features
  makeIntegerParam("nodesize", lower = 1, upper = 5),
  makeIntegerParam("maxnodes", lower = 5, upper = 20))

randSearch <- makeTuneControlRandom(maxit = 5)
cvForTuning <- makeResampleDesc("CV", iters = 3, stratify = T)

tunedForestPars <- tuneParams(forest, task = ml_task,
                              resampling = cvForTuning,
                              par.set = forestParamSpace,
                              control = randSearch)

tunedForestPars


#Crossvalidation
cv <- makeResampleDesc("CV", iters = 3, stratify = TRUE)
cv_RF <- resample(learner = forest, task = ml_task, par.vals = tunedForestPars$x,
                  resampling = cv, measures = list(mmce, acc))

#Performance
conf_matrix_RF <- calculateConfusionMatrix(cv_RF$pred, relative = FALSE)
conf_matrix_RF <- cbind(rownames(conf_matrix_RF$result), as_tibble(conf_matrix_RF$result))
export(conf_matrix_RF, file = "TwCl4_confmatrix_RF.xlsx")

rf_predictions <- getRRPredictionList(cv_RF)
rf_1_fold <- as.data.frame(rf_predictions$test$`1`)
rf_2_fold <- as.data.frame(rf_predictions$test$`2`)
rf_3_fold <- as.data.frame(rf_predictions$test$`3`)

export(rf_1_fold, "pred_TwCl4_rf1.xlsx")
export(rf_2_fold, "pred_TwCl4_rf2.xlsx")
export(rf_3_fold, "pred_TwCl4_rf3.xlsx")