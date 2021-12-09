# ST443 Group Assignment
# Authors:
# 

# Setting work directory
setwd("C:/Users/famil/MEGA/msc_financial_statistics/Machine Learning/Group Project")

# Reading file
df=read.csv("final_regression_data_all.csv",header=T)

#Installing packages
install.packages("dplyr")
install.packages("forecast")
install.packages("pls")
install.packages("caret")
install.packages("glmnet")
install.packages("MLmetrics")
library(MLmetrics)
library(caret)
library(glmnet)
library(pls)
library(forecast)
library(dplyr)

# Filtering shares with complete data (169 months)
conta <- df %>% count(df$stock_name)
colnames(conta) <- c("stocks", "n")

stock169 <- conta[which(conta$n == 169),]

mm = matrix(0,nrow=1570,ncol=1)
for (i in 1:1570) {
  mm[i] <- as.character(stock169$stocks[i])
}

datalist = list()

for (j in 1:1570){
  datalist[[j]] <- df[which(df$stock_name == mm[j]),]    
}

big_data <- bind_rows(datalist)


# Detecting and removing outliers in the monthly returns Time Series

gato = matrix(0,nrow=169,ncol=1)
dodo =list()
for (j in 1:1570){
  gato <- big_data[which(big_data$stock_name == mm[j]),] 
  gato$monthly_return <- tsclean(gato$monthly_return)
  dodo[[j]] <- gato
}

clean_big_data <- bind_rows(dodo)

# Train-test split: hold-out method

train <- clean_big_data[1:103,]
test <- clean_big_data[104:169,]
X_train <- train[,2:106]
X_test <- test[,2:106]
y_train <- train[,1]
y_test <- test[,1]

## Performing PCR

# Training the model
hh=list()
for (j in 1:1570){
  itrain <- clean_big_data[which(clean_big_data$stock_name == mm[j]),]
  train <- itrain[1:103,4:111]
  test <- itrain[104:169,4:111]
  pcr_model <- pcr(monthly_return~., data=train, scale = TRUE, validation = "CV", ncomp=3)
  hh[[i]] <- pcr_model$scores
}

pcr_model2 <- pcr(monthly_return~., data=train, scale = TRUE, validation = "CV", ncomp=2)

# Testing the model
pcr_pred <- predict(pcr_model2, test, ncomp=2)

RMSE(pcr_pred,y_test)

X_test2 <- X_test[,variables]

linear_model <- lm(y_train~X_train2)

lm_pred <- predict(linear_model, newx = X_test2)

y_test

RMSE(lm_pred,y_test)