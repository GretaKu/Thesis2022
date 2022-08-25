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

# ML transcripts --------------------------------------------------------------

load("transcript_cleavage_3.RData")
x <- x %>% filter(Text3 != "")

x$text <- tolower(x$Text3)

# 2. Balance to reduce size (7-point-scale)  ---------------------------
x_7 <- x %>% mutate(
  ideology_scale7 = ifelse(cleavage_3_10 == 9 | cleavage_3_10 == 10, "7:Strong Agree",
                           ifelse(cleavage_3_10 == 7 | cleavage_3_10 == 8, "6:Agree",
                                  ifelse(cleavage_3_10 == 6, "5:Moderate Agree",
                                         ifelse(cleavage_3_10 == 5, "4:Neutral",
                                                ifelse(cleavage_3_10 == 4, "3:Moderate Disagree",
                                                       ifelse(cleavage_3_10 == 3 | cleavage_3_10 == 2, "2:Disagree",
                                                              ifelse(cleavage_3_10 == 1 | cleavage_3_10 == 0, "1:Strong Disagree", NA))))))),
  ideology_scale7 = factor(ideology_scale7))

frq(x_7$cleavage_3_10)
frq(x_7$ideology_scale7) # Manual Check if groups are correctly allocated

transcript_cleavage_3_doc_7 <- data.frame(frq(x_7$ideology_scale7))

transcript_cleavage_3 = NULL
y = NULL
vec = c("2:Disagree", "3:Moderate Disagree", "4:Neutral", 
        "5:Moderate Agree", "6:Agree")
for (i in vec) {
  y <- x_7 %>% 
    filter(ideology_scale7 == i) %>% 
    sample_n(625)
  
  transcript_cleavage_3 <- rbind(y, transcript_cleavage_3)
}

frq(transcript_cleavage_3$ideology_scale7)

#Removing extra scale to avoid error in coding
transcript_cleavage_3 <- transcript_cleavage_3 %>% select(-cleavage_3_10, -cleavage_3, -Text3)

# Create DFM  -----------------------------------------------------------------
transcript_cleavage_3 <- corpus(transcript_cleavage_3)

# Further cleaning procedure
transcript_cleavage_32 <- tokens(transcript_cleavage_3 , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                                 split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
transcript_cleavage_32 <- tokens_remove(transcript_cleavage_32, c(stopwords("de"), "dass"))

transcript_cleavage_32 <- tokens_remove(transcript_cleavage_32, c("0*"))
transcript_cleavage_32 <- tokens_wordstem(transcript_cleavage_32, language = "german")
transcript_cleavage_32 <- dfm(transcript_cleavage_32)
topfeatures(transcript_cleavage_32 , 1000)  # 1000 top words

# Trimming 1: Remove features that only appear in 2 Tweet
transcript_cleavage_32 <- dfm_trim(transcript_cleavage_32 , min_docfreq = 15, verbose=TRUE)
transcript_cleavage_32  <- dfm_remove(transcript_cleavage_32 , min_nchar = 2)
topfeatures(transcript_cleavage_32 , 20)  # 20 top words

# Trimming 2: Remove doc with only 0
transcript_cleavage_32[ntoken(transcript_cleavage_32) == 0,]
transcript_cleavage_32  <-  transcript_cleavage_32[ntoken(transcript_cleavage_32) != 0,]
transcript_cleavage_32[ntoken(transcript_cleavage_32) == 0,]



transcript_cleavage_3_df <- transcript_cleavage_32 %>% 
  quanteda::convert(to = "data.frame") %>% 
  cbind(docvars(transcript_cleavage_32)) %>% 
  select(-doc_id)


# solving problem of features starting with digits eg 2x
names(transcript_cleavage_3_df) <- make.names(names(transcript_cleavage_3_df))

frq(transcript_cleavage_3_df$ideology_scale7)
transcript_cleavage_3_fea_7 <- data.frame(frq(transcript_cleavage_3_df$ideology_scale7))

# Task
ml_task <- makeClassifTask(data = transcript_cleavage_3_df, target = "ideology_scale7")


# kNN ---------------------------------------------------------------------
parallelStartSocket(cpus = detectCores())

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
cv <- makeResampleDesc("CV", iters = 3, stratify = TRUE)
cv_knn <- resample(learner = knn, task = ml_task,
                   resampling = cv, measures = list(mmce, acc))
capture.output(cv_knn, file = "TrC3_knn.txt")

#Performance
conf_matrix_knn <- calculateConfusionMatrix(cv_knn$pred, relative = FALSE)
conf_matrix_knn <- cbind(rownames(conf_matrix_knn$result), as_tibble(conf_matrix_knn$result))
export(conf_matrix_knn, file = "TrC3_confmatrix_knn.xlsx")


# Naive Bays --------------------------------------------------------------

#Set Up Model
bayes <- makeLearner("classif.naiveBayes")
bayesModel <- train(bayes, ml_task)


#CV Procedure
kFold <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)

bayesCV <- resample(learner = bayes, task = ml_task,
                    resampling = kFold, measures = list(mmce, acc))
capture.output(bayesCV, file = "TrC3_NB.txt")

#Performance
conf_matrix_NB <- calculateConfusionMatrix(bayesCV$pred, relative = FALSE)
conf_matrix_NB <- cbind(rownames(conf_matrix_NB$result), as_tibble(conf_matrix_NB$result))
export(conf_matrix_NB, file = "TrC3_confmatrix_NB.xlsx")


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
capture.output(cvWithTuning, file = "TrC3_SVM.txt")

#Performance
conf_matrix_SVM <- calculateConfusionMatrix(cvWithTuning$pred, relative = FALSE)
conf_matrix_SVM <- cbind(rownames(conf_matrix_SVM$result), as_tibble(conf_matrix_SVM$result))
export(conf_matrix_SVM, file = "TrC3_confmatrix_SVM.xlsx")


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

#Crossvalidation
cv <- makeResampleDesc("CV", iters = 3, stratify = TRUE)
cv_RF <- resample(learner = forest, task = ml_task, par.vals = tunedForestPars$x,
                  resampling = cv, measures = list(mmce, acc))
capture.output(cv_RF, file = "TrC3_RF_300.txt")

#Performance
conf_matrix_RF <- calculateConfusionMatrix(cv_RF$pred, relative = FALSE)
conf_matrix_RF <- cbind(rownames(conf_matrix_RF$result), as_tibble(conf_matrix_RF$result))
export(conf_matrix_RF, file = "TrC3_confmatrix_RF_300.xlsx")


## RETRY TREES = 500
forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 500, upper = 500),
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

#Crossvalidation
cv <- makeResampleDesc("CV", iters = 3, stratify = TRUE)
cv_RF <- resample(learner = forest, task = ml_task, par.vals = tunedForestPars$x,
                  resampling = cv, measures = list(mmce, acc))
capture.output(cv_RF, file = "TrC3_RF_500.txt")

#Performance
conf_matrix_RF <- calculateConfusionMatrix(cv_RF$pred, relative = FALSE)
conf_matrix_RF <- cbind(rownames(conf_matrix_RF$result), as_tibble(conf_matrix_RF$result))
export(conf_matrix_RF, file = "TrC3_confmatrix_RF_500.xlsx")

# Export Frequencies ------------------------------------------------------
library(rio)
export(transcript_cleavage_3_doc_7, "tweets_cleavage_3_finalDocs.xlsx")
export(transcript_cleavage_3_fea_7, "tweets_cleavage_3_finalDocs.xlsx")
