rm(list = ls())
setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/predictions")

library(rio)
library(tidyverse)

scores <- import("METRICS_summary.xlsx")

scores2 <- scores %>% mutate(text = recode(text, 
                                           "Tr"="Transcript",
                                           "Tw"="Twitter"),
                             
                             ideology = recode(ideology,
                                            "Se_"= "Ideology Scale (SELECTS)",
                                            "Cl1"= "Cleavage 1: Open foreign policy",
                                            "Cl2"= "Cleavage 2: Liberal economic policy",
                                            "Cl4"= "Cleavage 4: Law & order",
                                            "Cl5"= "Cleavage 5: Restrictive migration policy",
                                            "Cl6"= "Cleavage 6: Environmental protection",
                                            "Cl7"= "Cleavage 7: Welfare state cleavage",
                                            "Cl8"= "Cleavage 8: Liberal society"),
                             
                             algorithm = recode(algorithm,
                                               "_nb"= "NB",
                                               "_rf"= "RF",
                                               "knn"= "kNN",
                                               "svm"= "SVM"),
                             
                             F1 = ifelse(is.na(F1), 0, F1),
                             Recall = ifelse(is.na(Recall), 0, Recall),
                             Precision = ifelse(is.na(Precision), 0, Precision))
                             
                            


setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/results plots")
# Best Algorithm ----------------------------------------------------------

scores2  %>%
  ggplot() +
  geom_col(aes(x = text, y = F1, fill = text), position = "dodge2") +
  geom_text(aes(x = text, y = F1, label = round(F1,3)), vjust = 1.3) +
  geom_hline(yintercept = 0.14) +
  ggplot2::annotate(geom = "text", x = 1, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2)+
  facet_grid(ideology~algorithm, switch = "y") +
  theme_bw()+
  labs(title = "F1 Scores of predicting ideological positioning by text and algorithm",
                  x = "", y = "F1 Score") +
  theme(strip.placement = "outside", legend.position="none")+
  ylim(c(0,0.32))

ggsave("F1_overview.png", height = 15, width = 11)


# Best Text ---------------------------------------------------------------

scores2 %>% filter(ideology == "Ideology Scale (SELECTS)") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Ideology Scale (SELECTS)") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_SELECTS_text_algos.png", height = 5, width = 8)



scores2 %>% filter(ideology == "Cleavage 1: Open foreign policy") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 1: Open foreign policy") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav1_text_algos.png", height = 5, width = 8)


scores2 %>% filter(ideology == "Cleavage 2: Liberal economic policy") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 2: Liberal economic policy") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav2_text_algos.png", height = 5, width = 8)

scores2 %>% filter(ideology == "Cleavage 4: Law & order") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 4: Law & order") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav4_text_algos.png", height = 5, width = 8)

scores2 %>% filter(ideology == "Cleavage 5: Restrictive migration policy") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 5: Restrictive migration policy") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav5_text_algos.png", height = 5, width = 8)


scores2 %>% filter(ideology == "Cleavage 6: Environmental protection") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 1.2, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 6: Environmental protection") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav6_text_algos.png", height = 5, width = 8)



scores2 %>% filter(ideology == "Cleavage 7: Welfare state cleavage") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 3.6, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 7: Welfare state cleavage") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav7_text_algos.png", height = 5, width = 8)

scores2 %>% filter(ideology == "Cleavage 8: Liberal society") %>% 
  ggplot() +
  geom_col(aes(x = algorithm, y = F1, fill = text), position = "dodge2") +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = algorithm, y = F1, label = round(F1,3)), vjust = 1.3)+
  annotate(geom = "text", x = 3.6, y = 0.15, label = "Baseline F1=0.14", size = 3.5, fontface = 2) +
  facet_grid(~text, switch = "y") +
  labs(title = "",
       x = "", y = "F1 Score",
       subtitle = "Cleavage 8: Liberal society") +
  theme(strip.placement = "outside")+
  theme_bw()+
  ylim(c(0,0.32))

ggsave("F1_Cleav8_text_algos.png", height = 5, width = 8)



# Classes Prediction ------------------------------------------------------
setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/predictions")

classses <- import("METRICS_class_metrics.xlsx")

classses2 <- classses %>% mutate(text = recode(text, 
                                           "Tr"="Transcript",
                                           "Tw"="Twitter"),
                             
                             ideology = recode(ideology,
                                               "Se_"= "Ideology Scale (SELECTS)",
                                               "Cl1"= "Cleavage 1: Open foreign policy",
                                               "Cl2"= "Cleavage 2: Liberal economic policy",
                                               "Cl4"= "Cleavage 4: Law & order",
                                               "Cl5"= "Cleavage 5: Restrictive migration policy",
                                               "Cl6"= "Cleavage 6: Environmental protection",
                                               "Cl7"= "Cleavage 7: Welfare state cleavage",
                                               "Cl8"= "Cleavage 8: Liberal society"),
                             
                             class = recode(class,
                                               "Class: 1"= "1:Strong Left",
                                               "Class: 2"= "2:Left",
                                               "Class: 3"= "3:Moderate Left",
                                               "Class: 4"= "4:Neutral",
                                               "Class: 5"= "5:Moderate Right",
                                               "Class: 6"= "6:Right",
                                               "Class: 7"= "7:Strong Right"),
                             
                             algorithm = recode(algorithm,
                                                "_nb"= "NB",
                                                "_rf"= "RF",
                                                "knn"= "kNN",
                                                "svm"= "SVM"),
                             
                             F1 = ifelse(is.na(F1), 0, F1),
                             Recall = ifelse(is.na(Recall), 0, Recall),
                             Precision = ifelse(is.na(Precision), 0, Precision))

classses2 <- classses2 %>% filter(ideology == "Ideology Scale (SELECTS)")

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data/results plots")

group.colors <- c("1:Strong Left" = "#8b0000", 
                  "2:Left" = "#63666A", 
                  "3:Moderate Left" ="#63666A", 
                  "4:Neutral" = "#63666A", 
                  "5:Moderate Right" = "#63666A",
                  "6:Right" = "#63666A",
                  "7:Strong Right" = "#8b0000")


classses2 %>% filter(text == "Transcript") %>% 
  ggplot()+
  geom_col(aes(x = class, y = F1, fill = class), position = "dodge2") +
  theme_bw() +
  labs(title = "Transcript data: Class prediction") +
  facet_wrap(~algorithm) +
  scale_fill_manual(values=group.colors) +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = class, y = F1, label = round(F1,3)), vjust = 1.3, size = 2.9, color = "white")+
  ggplot2::annotate(geom = "text", x = 6.2, y = 0.15, label = "Baseline F1=0.14", size = 3, fontface = 2) +
  labs(x = "", y = "F1 score") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylim(c(0,0.34))
ggsave("F1_class_trans.png", height = 8, width = 7.3)


classses2 %>% filter(text == "Twitter") %>% 
  ggplot()+
  geom_col(aes(x = class, y = F1, fill = class), position = "dodge2") +
  theme_bw() +
  labs(title = "Twitter data: Class prediction") +
  facet_wrap(~algorithm) +
  scale_fill_manual(values=group.colors) +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = class, y = F1, label = round(F1,3)), vjust = 1.3, size = 2.9, color = "white")+
  annotate(geom = "text", x = 6.2, y = 0.15, label = "Baseline F1=0.14", size = 3, fontface = 2) +
  labs(x = "", y = "F1 score")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylim(c(0,0.34))
ggsave("F1_class_twit.png", height = 8, width = 7.3)











## TRANSCRIPT: SVM and NB Deepdive
deep_class <- classses2 %>% filter(text == "Transcript")  
deep_class <- deep_class %>% select(class, text, ideology, algorithm, F1, Recall, Precision) %>% 
  pivot_longer(cols = c(5:7), names_to = "var", values_to = "val")

deep_class %>%
  ggplot(aes(x = class, y = val, color = var, group = var))+
  geom_line(size = 1) +
  theme_bw() +
  labs(title = "Transcipt data: Class prediction") +
  facet_wrap(~algorithm) +
  geom_hline(yintercept = 0.14) +
  geom_text(aes(x = class, y = val, label = round(val,3)), vjust = 1.3, size = 2.9, color = "darkgrey")+
  annotate(geom = "text", x = 1.5, y = 0.135, label = "Baseline F1=0.14", size = 2.5) +
  labs(x = "", y = "F1 score") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("F1_class_trans_SVM_NB.png", height = 5, width = 7)


# Heatmap Cor -------------------------------------------------------------

setwd("C:/Users/Greta/OneDrive/Dokumente/UNILU/Masterarbeit/Data")

library(rio)
library(tidyverse)
library(psych)

ideology <- import("ideology2.RData")


# Heatmap -----------------------------------------------------------------
ideology <- na.omit(ideology[,c(8:15)])
cormat <- round(cor(ideology),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()










