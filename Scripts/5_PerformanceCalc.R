rm(list = ls())

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/predictions")
library(rio)
library(tidyverse)
library(sjmisc)
library(MLmetrics)
library(readxl)
library(mltest)

# Reading in all Prediction Datasets --------------------------------------

# List all preiction files for every model:
#  algorithm * ideologyvar * text * fold 1-3 = 
#  4*8*2*3 = 192 files
filenames <- list.files()

# Create DF with all files and variable labelling the file
df <- map_dfr(filenames, ~ read_excel(.x) %>% mutate(file = paste0( .x)))


# Improving Dataframe before creating metric ------------------------------

data <- df %>% # Creating same scale for smartvote and selects
  mutate(truth = factor(recode(truth, "1:Strong Disagree" = 1, "1:Strong Left" = 1,
                        "2:Disagree" = 2, "2:Left" = 2,
                        "3:Moderate Disagree" = 3, "3:Moderate Left" = 3,
                        "4:Neutral" = 4, 
                        "5:Moderate Agree" = 5, "5:Moderate Right" = 5,
                        "6:Agree" = 6, "6:Right" = 6,
                        "7:Strong Agree" = 7, "7:Strong Right" = 7)),
         
         response = factor(recode(response, "1:Strong Disagree" = 1, "1:Strong Left" = 1,
                        "2:Disagree" = 2, "2:Left" = 2,
                        "3:Moderate Disagree" = 3, "3:Moderate Left" = 3,
                        "4:Neutral" = 4, 
                        "5:Moderate Agree" = 5, "5:Moderate Right" = 5,
                        "6:Agree" = 6, "6:Right" = 6,
                        "7:Strong Agree" = 7, "7:Strong Right" = 7)))

# Improving Filename 
data$file <- str_remove(data$file, ".xlsx")

# exporting the number of cases per folds, before merging all folds of each model (3 folds to 1 dataset)
export(frq(data$file), "frequencies_pred_folds.xlsx") 

# Removing fold number so all records for the folds are combined
# Remeber: Each fold has been once a testset while training on other 2 folds, all 3 folds combined = all testdata
data$file <- str_sub(data$file, 6, nchar(data$file)-1)
files <- unique(data$file) # Shows all 64 models (4*8*2)



# Creating Metrics for each CLASS of each dataset -------------------------
options(scipen = 999)

class_all = NULL
overall_all = NULL
confmatrix_all = NULL

for (i in files) {
  file <- data %>% filter(file == i) # Filtering to each dataset
  
  x <- caret::confusionMatrix(file$response, file$truth) # calculate measures
  
  # Class prediction metrics
  class <- data.frame(x$byClass)
  class <- rownames_to_column(class, "class")
  class$file <- print(i)
  class_all <- rbind(class_all, class)
  
  # Overall significance testing
  overall <- data.frame(x$overall)
  overall <- rownames_to_column(overall, "metric") # Keep metric index
  overall <- pivot_wider(overall, id_cols = c(1:2), names_from = metric, values_from = x.overall)
  overall <- overall %>% mutate(
    file = print(i))
  overall_all <- rbind(overall_all, overall)
  
  # ConfMatrix
  confmatrix <- as.data.frame(x$table)
  confmatrix$file <- print(i)
  confmatrix_all <- rbind(confmatrix_all, confmatrix)
}

# Altering file name again for plots
overall_all2 <- overall_all %>% 
  mutate(
    text = str_sub(file, 1, 2),
    ideology = str_sub(file, 3,5),
    algorithm = str_sub(file, nchar(file)-2, nchar(file))
  )

class_all2 <- class_all %>% 
  mutate(
    text = str_sub(file, 1, 2),
    ideology = str_sub(file, 3,5),
    algorithm = str_sub(file, nchar(file)-2, nchar(file))
  )

export(overall_all2, "METRICS_overall_significance.xlsx")
export(class_all2, "METRICS_class_metrics.xlsx")
export(confmatrix_all, "METRICS_confmatrix.xlsx")


# Creating Metrics for each CLASS of each dataset -------------------------
summary_metrics <- class_all2 %>% group_by(text, ideology, algorithm) %>% 
  summarise(n = n(),
            F1 = mean(F1),
            Recall = mean(Recall),
            Precision = mean(Precision))

export(summary_metrics, "METRICS_summary.csv")

