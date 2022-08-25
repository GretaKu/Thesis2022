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

# ML transcripts --------------------------------------------------------------

load("transcript_cleavage_6.RData")
x <- x %>% filter(Text3 != "")

x$text <- tolower(x$Text3)

# 2. Balance to reduce size (7-point-scale)  ---------------------------
x_7 <- x %>% mutate(
  ideology_scale7 = ifelse(cleavage_6_10 == 9 | cleavage_6_10 == 10, "7:Strong Agree",
                           ifelse(cleavage_6_10 == 7 | cleavage_6_10 == 8, "6:Agree",
                                  ifelse(cleavage_6_10 == 6, "5:Moderate Agree",
                                         ifelse(cleavage_6_10 == 5, "4:Neutral",
                                                ifelse(cleavage_6_10 == 4, "3:Moderate Disagree",
                                                       ifelse(cleavage_6_10 == 3 | cleavage_6_10 == 2, "2:Disagree",
                                                              ifelse(cleavage_6_10 == 1 | cleavage_6_10 == 0, "1:Strong Disagree", NA))))))),
  ideology_scale7 = factor(ideology_scale7))

frq(x_7$cleavage_6_10)
frq(x_7$ideology_scale7) # Manual Check if groups are correctly allocated

transcript_cleavage_6_doc_7 <- data.frame(frq(x_7$ideology_scale7))

transcript_cleavage_6 = NULL
y = NULL
vec = c("1:Strong Disagree", "2:Disagree", "3:Moderate Disagree", "4:Neutral", 
        "5:Moderate Agree", "6:Agree", "7:Strong Agree")
for (i in vec) {
  y <- x_7 %>% 
    filter(ideology_scale7 == i) %>% 
    sample_n(827)
  
  transcript_cleavage_6 <- rbind(y, transcript_cleavage_6)
}

frq(transcript_cleavage_6$ideology_scale7)

#Removing extra scale to avoid error in coding
transcript_cleavage_6 <- transcript_cleavage_6 %>% select(-cleavage_6_10, -cleavage_6, -Text3)

# Create DFM  -----------------------------------------------------------------
transcript_cleavage_6 <- corpus(transcript_cleavage_6)

# Further cleaning procedure
transcript_cleavage_62 <- tokens(transcript_cleavage_6 , remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, 
                                 split_hyphens = TRUE, remove_separators = TRUE, remove_url = TRUE)
transcript_cleavage_62 <- tokens_remove(transcript_cleavage_62, c(stopwords("de"), "dass"))

transcript_cleavage_62 <- tokens_remove(transcript_cleavage_62, c("0*"))
transcript_cleavage_62 <- tokens_wordstem(transcript_cleavage_62, language = "german")
transcript_cleavage_62 <- dfm(transcript_cleavage_62)
topfeatures(transcript_cleavage_62 , 1000)  # 1000 top words

# Trimming 1: Remove features that only appear in 2 Tweet
transcript_cleavage_62 <- dfm_trim(transcript_cleavage_62 , min_docfreq = 35, verbose=TRUE)
transcript_cleavage_62  <- dfm_remove(transcript_cleavage_62 , min_nchar = 2)
topfeatures(transcript_cleavage_62 , 20)  # 20 top words

# Trimming 2: Remove doc with only 0
transcript_cleavage_62[ntoken(transcript_cleavage_62) == 0,]
transcript_cleavage_62  <-  transcript_cleavage_62[ntoken(transcript_cleavage_62) != 0,]
transcript_cleavage_62[ntoken(transcript_cleavage_62) == 0,]



transcript_cleavage_6_df <- transcript_cleavage_62 %>% 
  quanteda::convert(to = "data.frame") %>% 
  cbind(docvars(transcript_cleavage_62)) %>% 
  select(-doc_id)


# solving problem of features starting with digits eg 2x
names(transcript_cleavage_6_df) <- make.names(names(transcript_cleavage_6_df))

frq(transcript_cleavage_6_df$ideology_scale7)
transcript_cleavage_6_fea_7 <- data.frame(frq(transcript_cleavage_6_df$ideology_scale7))

# Task
ml_task <- makeClassifTask(data = transcript_cleavage_6_df, target = "ideology_scale7")


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

#Performance
conf_matrix_knn <- calculateConfusionMatrix(cv_knn$pred, relative = FALSE)
conf_matrix_knn <- cbind(rownames(conf_matrix_knn$result), as_tibble(conf_matrix_knn$result))
export(conf_matrix_knn, file = "TrCl6_confmatrix_knn.xlsx")

knn_predictions <- getRRPredictionList(cv_knn)
knn_1_fold <- as.data.frame(knn_predictions$test$`1`)
knn_2_fold <- as.data.frame(knn_predictions$test$`2`)
knn_3_fold <- as.data.frame(knn_predictions$test$`3`)

export(knn_1_fold, "pred_TrCl6_knn1.xlsx")
export(knn_2_fold, "pred_TrCl6_knn2.xlsx")
export(knn_3_fold, "pred_TrCl6_knn3.xlsx")

# Naive Bays --------------------------------------------------------------

#Set Up Model
bayes <- makeLearner("classif.naiveBayes")
bayesModel <- train(bayes, ml_task)


#CV Procedure
kFold <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)

bayesCV <- resample(learner = bayes, task = ml_task,
                    resampling = kFold, measures = list(mmce, acc))

#Performance
conf_matrix_NB <- calculateConfusionMatrix(bayesCV$pred, relative = FALSE)
conf_matrix_NB <- cbind(rownames(conf_matrix_NB$result), as_tibble(conf_matrix_NB$result))
export(conf_matrix_NB, file = "TrCl6_confmatrix_NB.xlsx")

nb_predictions <- getRRPredictionList(bayesCV)
nb_1_fold <- as.data.frame(nb_predictions$test$`1`)
nb_2_fold <- as.data.frame(nb_predictions$test$`2`)
nb_3_fold <- as.data.frame(nb_predictions$test$`3`)

export(nb_1_fold, "pred_TrCl6_nb1.xlsx")
export(nb_2_fold, "pred_TrCl6_nb2.xlsx")
export(nb_3_fold, "pred_TrCl6_nb3.xlsx")

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
export(conf_matrix_SVM, file = "TrCl6_confmatrix_SVM.xlsx")

svm_predictions <- getRRPredictionList(cvWithTuning)
svm_1_fold <- as.data.frame(svm_predictions$test$`1`)
svm_2_fold <- as.data.frame(svm_predictions$test$`2`)
svm_3_fold <- as.data.frame(svm_predictions$test$`3`)

export(svm_1_fold, "pred_TrCl6_svm1.xlsx")
export(svm_2_fold, "pred_TrCl6_svm2.xlsx")
export(svm_3_fold, "pred_TrCl6_svm3.xlsx")

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

#Performance
conf_matrix_RF <- calculateConfusionMatrix(cv_RF$pred, relative = FALSE)
conf_matrix_RF <- cbind(rownames(conf_matrix_RF$result), as_tibble(conf_matrix_RF$result))
export(conf_matrix_RF, file = "TrCl6_confmatrix_RF.xlsx")

rf_predictions <- getRRPredictionList(cv_RF)
rf_1_fold <- as.data.frame(rf_predictions$test$`1`)
rf_2_fold <- as.data.frame(rf_predictions$test$`2`)
rf_3_fold <- as.data.frame(rf_predictions$test$`3`)

export(rf_1_fold, "pred_TrCl6_rf1.xlsx")
export(rf_2_fold, "pred_TrCl6_rf2.xlsx")
export(rf_3_fold, "pred_TrCl6_rf3.xlsx")
