rm(list = ls())
setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/clean data")
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

load("twitter_selects.RData")

x <- rowid_to_column(x, "id_text") #rownames to connect results to text


# 2. Balance to reduce size (7-point-scale)  ---------------------------
x_7 <- x %>% mutate(
  ideology_scale7 = ifelse(ideology_scale == 9 | ideology_scale == 10, "7:Strong Right",
                           ifelse(ideology_scale == 7 | ideology_scale == 8, "6:Right",
                                  ifelse(ideology_scale == 6, "5:Moderate Right",
                                         ifelse(ideology_scale == 5, "4:Neutral",
                                                ifelse(ideology_scale == 4, "3:Moderate Left",
                                                       ifelse(ideology_scale == 3 | ideology_scale == 2, "2:Left",
                                                              ifelse(ideology_scale == 1 | ideology_scale == 0, "1:Strong Left", NA))))))),
  ideology_scale7 = factor(ideology_scale7))

frq(x_7$ideology_scale)
frq(x_7$ideology_scale7) # Manual Check if groups are correctly allocated

tweets_selects_doc_7 <- data.frame(frq(x_7$ideology_scale7))

twitter_selects = NULL
y = NULL
vec = c("1:Strong Left", "2:Left", "3:Moderate Left", "4:Neutral", 
        "5:Moderate Right", "6:Right", "7:Strong Right") 
for (i in vec) {
  y <- x_7 %>% 
    filter(ideology_scale7 == i) %>% 
    sample_n(1473)
  
  twitter_selects <- rbind(y, twitter_selects)
}

frq(twitter_selects$ideology_scale7)

#Removing extra scale to avoid error in coding
twitter_selects <- twitter_selects %>% select(-ideology_scale)

# Create DFM  -----------------------------------------------------------------
twitter_selects <- corpus(twitter_selects)

# Further cleaning procedure
twitter_selects2 <- tokens(twitter_selects , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                           split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
twitter_selects2 <- tokens_remove(twitter_selects2, c(stopwords("en"), stopwords("de"), "dass"))

twitter_selects2 <- tokens_remove(twitter_selects2, c("0*"))
twitter_selects2 <- tokens_wordstem(twitter_selects2, language = "german")
twitter_selects2 <- dfm(twitter_selects2)
topfeatures(twitter_selects2 , 1000)  # 1000 top words

# Trimming 1: Remove features that only appear in 2 Tweet
twitter_selects2 <- dfm_trim(twitter_selects2 , min_docfreq = 15, verbose=TRUE)
twitter_selects2  <- dfm_remove(twitter_selects2 , min_nchar = 2)
topfeatures(twitter_selects2 , 20)  # 20 top words

# Trimming 2: Remove doc with only 0
twitter_selects2[ntoken(twitter_selects2) == 0,]
twitter_selects2  <-  twitter_selects2[ntoken(twitter_selects2) != 0,]
twitter_selects2[ntoken(twitter_selects2) == 0,]



twitter_selects_df <- twitter_selects2 %>% 
  quanteda::convert(to = "data.frame") %>% 
  cbind(docvars(twitter_selects2)) %>% 
  rowid_to_column("id_pred") #adding ID to link Pred results after prediction

export(twitter_selects_df, "Twitter_PrePrediction_QUAL.xlsx")


twitter_selects_df <- twitter_selects_df %>% 
  select(-doc_id, -id_pred, -id_text)


# solving problem of features starting with digits eg 2x
names(twitter_selects_df) <- make.names(names(twitter_selects_df))

frq(twitter_selects_df$ideology_scale7)
tweets_selects_fea_7 <- data.frame(frq(x$ideology_scale7))

parallelStartSocket(cpus = detectCores())
# kNN ---------------------------------------------------------------------
# Task
ml_task <- makeClassifTask(data = twitter_selects_df, target = "ideology_scale7")

#Hyper param Tuning - 
gridSearch <- makeTuneControlGrid()
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:5))

cv <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

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
export(conf_matrix_knn, file = "TrSe_confmatrix_knn.xlsx")

knn_predictions <- getRRPredictionList(cv_knn)
knn_1_fold <- as.data.frame(knn_predictions$test$`1`)
knn_2_fold <- as.data.frame(knn_predictions$test$`2`)
knn_3_fold <- as.data.frame(knn_predictions$test$`3`)

export(knn_1_fold, "pred_TwSe_kNN1.xlsx")
export(knn_2_fold, "pred_TwSe_kNN2.xlsx")
export(knn_3_fold, "pred_TwSe_kNN3.xlsx")

# Naive Bays --------------------------------------------------------------

#Task
ml_task <- makeClassifTask(data = twitter_selects_df, target = "ideology_scale7")

#Set Up Model
bayes <- makeLearner("classif.naiveBayes")

#CV Procedure
bayesCV <- resample(learner = bayes, task = ml_task,
                    resampling = cv, measures = list(mmce, acc))

#Performance
conf_matrix_nb <- calculateConfusionMatrix(bayesCV$pred, relative = FALSE)
conf_matrix_nb <- cbind(rownames(conf_matrix_nb$result), as_tibble(conf_matrix_nb$result))
export(conf_matrix_nb, file = "TrSe_confmatrix_nb.xlsx")

nb_predictions <- getRRPredictionList(bayesCV)
nb_1_fold <- as.data.frame(nb_predictions$test$`1`)
nb_2_fold <- as.data.frame(nb_predictions$test$`2`)
nb_3_fold <- as.data.frame(nb_predictions$test$`3`)

export(nb_1_fold, "pred_TwSe_nb1.xlsx")
export(nb_2_fold, "pred_TwSe_nb2.xlsx")
export(nb_3_fold, "pred_TwSe_nb3.xlsx")


# SVM ---------------------------------------------------------------------

ml_task
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

parallelStartSocket(cpus = detectCores())

tunedSvmPars <- tuneParams("classif.svm", task = ml_task, 
                           resampling = cvForTuning,
                           par.set = svmParamSpace,
                           control = randSearch)


# Show best parameters
tunedSvmPars

# CV Model Building Process
outer <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning,
                              par.set = svmParamSpace,
                              control = randSearch)

cvWithTuning <- resample(svmWrapper, ml_task, resampling = outer)

#Performance
conf_matrix_svm <- calculateConfusionMatrix(cvWithTuning$pred, relative = FALSE)
conf_matrix_svm <- cbind(rownames(conf_matrix_svm$result), as_tibble(conf_matrix_svm$result))
export(conf_matrix_svm, file = "TrSe_confmatrix_svm.xlsx")

svm_predictions <- getRRPredictionList(cvWithTuning)
svm_1_fold <- as.data.frame(svm_predictions$test$`1`)
svm_2_fold <- as.data.frame(svm_predictions$test$`2`)
svm_3_fold <- as.data.frame(svm_predictions$test$`3`)

export(svm_1_fold, "pred_TwSe_svm1.xlsx")
export(svm_2_fold, "pred_TwSe_svm2.xlsx")
export(svm_3_fold, "pred_TwSe_svm3.xlsx")


# Random Forest -----------------------------------------------------------

forest <- makeLearner("classif.randomForest")

# Hyperparameter Tuning
forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 300, upper = 300),
  makeIntegerParam("mtry", lower = 10, upper = 100), #cause lots of features
  makeIntegerParam("nodesize", lower = 1, upper = 5),
  makeIntegerParam("maxnodes", lower = 5, upper = 20))

randSearch <- makeTuneControlRandom(maxit = 20)
cvForTuning <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

tunedForestPars <- tuneParams(forest, task = ml_task,
                              resampling = cvForTuning,
                              par.set = forestParamSpace,
                              control = randSearch)
tunedForestPars

# Create Model
tunedForest <- setHyperPars(forest, par.vals = tunedForestPars$x)

# CV Model Building Process
outer <- makeResampleDesc("CV", iters = 3, stratify = TRUE)

forestWrapper <- makeTuneWrapper("classif.randomForest", resampling = cvForTuning,
                                 par.set = forestParamSpace,
                                 control = randSearch)
cvWithTuning <- resample(forestWrapper, ml_task, resampling = outer)

#Performance
conf_matrix_rf <- calculateConfusionMatrix(cvWithTuning$pred, relative = FALSE)
conf_matrix_rf <- cbind(rownames(conf_matrix_rf$result), as_tibble(conf_matrix_rf$result))
export(conf_matrix_rf, file = "TrSe_confmatrix_rf.xlsx")

rf_predictions <- getRRPredictionList(cvWithTuning)
rf_1_fold <- as.data.frame(rf_predictions$test$`1`)
rf_2_fold <- as.data.frame(rf_predictions$test$`2`)
rf_3_fold <- as.data.frame(rf_predictions$test$`3`)

export(rf_1_fold, "pred_TwSe_rf1.xlsx")
export(rf_2_fold, "pred_TwSe_rf2.xlsx")
export(rf_3_fold, "pred_TwSe_rf3.xlsx")

