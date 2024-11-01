

##########################################################################
library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/code/similarities1.xlsx")
model <-glmer(IsCorrect ~ sim_mean_max + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)


library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/code/similarities1.xlsx")
data$Condition2 <- factor(data$Condition2)
contrasts(data$Condition2) = contr.sum(4)
model <-glmer(IsCorrect ~ sim_mean_max* Condition2 + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/code/similarities1.xlsx")
model <-glmer(IsCorrect ~ sim_mean_mean + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/code/similarities1.xlsx")
model <-glmer(IsCorrect ~  sim_mean_max + sim_mean_std + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

filtered_df <- subset(data, Condition2 == "Talker-specific")
model <-glmer(IsCorrect ~  sim_mean_max  + (1|WorkerID) + (1|SentenceID), data = filtered_df, family=binomial)
summary(model)

