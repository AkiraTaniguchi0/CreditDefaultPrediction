setwd('C:/Users/16467/Documents/Coding/CreditDefault')
rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library("readxl")
library(caret)
library(MASS)
library(ggcorrplot)
library(car)

#https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients

df <- read_excel("default_data.xlsx")

#Function that turns first row into column names
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]}

df <- header.true(df)

#Remove ID column
df <- df[,-1]

#Rename column
colnames(df)[colnames(df) == "default payment next month"] ="Default"

#Change all columns from character to numeric
df[] <- lapply(df, function(x) as.numeric(as.character(x)))

#Feature Engineering
df$MARRIAGE[df$MARRIAGE == 0] <- 3
df$EDUCATION[df$EDUCATION == 0] <- 4
df$EDUCATION[df$EDUCATION == 5] <- 4
df$EDUCATION[df$EDUCATION == 6] <- 4

#Change columns to factor variables
df$SEX<- as.factor(df$SEX)
df$EDUCATION<- as.factor(df$EDUCATION)
df$MARRIAGE<- as.factor(df$MARRIAGE)

#Separate data into 70% training and 30% test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

full_model <- glm(Default~., family=binomial, data=train)
summary(full_model)
vif(full_model)

step_model <- full_model %>% stepAIC(trace = FALSE)
summary(step_model)
vif(step_model)

drops <- c("PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6")
df1 <- df[ , !(names(df) %in% drops)]

sample1 <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.7,0.3))
train1  <- df1[sample1, ]
test1   <- df1[!sample1, ]

full_model1 <- glm(Default~., family=binomial, data=train1)
summary(full_model1)
vif(full_model1)

step_model2 <- full_model1 %>% stepAIC(trace = FALSE)
summary(step_model2)
vif(step_model2)
coef(step_model2)



