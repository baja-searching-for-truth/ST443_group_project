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


## Post-LASSO

# Extracting covariances between factors and  returns

covar = matrix(0,nrow=1570,ncol=107)
avg_mr = matrix(0,nrow=1570,ncol=1)
X_stock <- clean_big_data[which(clean_big_data$stock_name == mm[1]),5:111]
for (i in 1:1570){
  y_stock <- clean_big_data[which(clean_big_data$stock_name == mm[i]),4]
  avg_mr[i,1] <- mean(y_stock)
  for (j in 1:106){
    covar[i,j] <- cov(X_stock[j], y_stock, method ="pearson")
  }
}
avg_mr <- as.matrix(avg_mr)
covar <- as.matrix(covar)

# Finding optimal lambda

lambdas <- 10^seq(-6,-1,by = 0.1)
lasso_reg = cv.glmnet(covar, avg_mr, alpha = 1, lambda = lambdas, standardize=TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min
lambda.1se <- lasso_reg$lambda.1se
lasso_reg$lambda.min
lasso_reg$lambda.1se
plot(lasso_reg)

# Performing Lasso Regularization
lasso_model <- glmnet(covar, avg_mr, alpha = 1, lambda = lambda.1se, standardize = TRUE)
coef(lasso_model)

variab <- c(3,6,11,12,13,15,20,21,24,25,28,29,41,42,43,45,51,52,53,59,60,62,64,68,70,72,76,79,83,86,88,89,92,93,94,96,97,98,99,101,103,104,105) 

# Subset of variables selected by the LASSO approach
covar_reg <- covar[,variab]

cdc <- colnames(clean_big_data)

cdc

# Train-Test split
set.seed(09122021)
tt_split <- sample(c(rep(1,0.7*nrow(covar)),rep(0,0.3*nrow(covar))))

X_train <- covar_reg[tt_split == 1,]
y_train <- avg_mr[tt_split == 1]
X_test <- covar_reg[tt_split == 0,]
y_test <- avg_mr[tt_split == 0]

# Perform cross-section OLS regression

train_merge <- data.frame(cbind(y_train,X_train))
test_merge <- data.frame(cbind(y_test,X_test))

linear_model <- lm(y_train~., data=train_merge)

linear_model$coefficients
result <- summary(linear_model)
cff <- result[["coefficients"]]

pred_lm <- predict(linear_model, newdata=test_merge,interval = "confidence")

RMSE(pred_lm,y_test)

## Elastic-Net regression

