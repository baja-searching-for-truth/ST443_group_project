library(purrr)
library(tidyr)
library(dplyr)
library(corrplot)
library(class)
library(leaps)

setwd("C:\\Users\\technoplanet\\Desktop\\LSE Projects\\ST443\\Group Project")

# read factor returns
data=read.csv("factor_returns.csv") 
data_filtered=data %>% 
  filter(X>=72)  %>% 
  select(-X)  %>% 
  select(-date) %>% 
  select(-RF)
# PCA
principal_output <-prcomp(data_filtered, scale=TRUE)
principal_variance <-principal_output$sdev^2
principal_variance_portion <-principal_variance/sum(principal_variance)
par(mfrow=c(1,2))
plot(principal_variance_portion, 
     xlab="Principal Component", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1), 
     type='l',
     cex.lab=0.8,
     lwd = 2)
plot(cumsum(principal_variance_portion), 
     xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1), 
     type='l',
     cex.lab=0.8,
     lwd = 2)
mtext("Variance Explained Against Number of Principal Components",                   # Add main title
      side = 3,
      line = - 2,
      outer = TRUE)

# Generate principal components
pca=princomp(data_filtered)
load=loadings(pca)
data_matrix=data.matrix(data_filtered)
loading_matrix=data.matrix(load[,])
pca_factors=data.frame(data_matrix %*% loading_matrix)
pca_factors$date=data %>% 
  filter(X>=72) %>% 
  select(date)
# merge components with stock returns
pca_factors$date=map(pca_factors$date[,1],function(x) format(as.Date(x),"%Y-%m-%d")) 
stock_trading_data=read.csv("cleaned_stock_return_with_factors.csv")
stock_trading_data_raw=stock_trading_data %>% 
  mutate(monthly_excess_return=monthly_return-RF) %>% 
  select(c("date","monthly_excess_return"))
excess_return_with_pca_factors=merge(stock_trading_data_raw,pca_factors,by="date") %>% 
  select(-date)

# Adding more components in regression and seeing how it fits
principal_component_number_grid=1:50
count_grid=1:length(principal_component_number_grid)
MSE=vector()
R_squared=vector()
for (i in count_grid)
{
  # select desired data and do the regression
  temp=excess_return_with_pca_factors[1:principal_component_number_grid[i]]
  lmodel=lm(monthly_excess_return~.,data=temp)
  MSE[i]=mean(lmodel$residuals^2)
  R_squared[i]=summary(lmodel)$r.squared

}
plot(principal_component_number_grid,
     MSE,
     type='l',
     xlab="Principal Components")
plot(principal_component_number_grid,
     R_squared,type='l',
     xlab="Principal Components")
mtext("Result of Regression on All Stocks",
      side=3,
      line=-2,
      outer = TRUE)

# Rolling window to find important factors in principal components
Rollingwindow <- function(data,D,A=1)
{
  loadings_temp=data %>% 
    princomp %>% 
    loadings
  load_temp=loadings_temp[,1] %>% data.frame
  names(load_temp)=c("loading")
  count_load_appearance <- load_temp %>% 
    mutate(count=0) %>% 
    select(-loading)
  factor_name=names(data_filtered)
  initial=1+D
  count=1
  series=list(0)
  fit=list(0)
  SE=vector()
  stop_loop_signal=FALSE
  continue=TRUE
  while ((initial<=dim(data)[1]) && (continue==TRUE))
  {
    data_this_window= data %>% slice((initial-D):(initial))
    PCA=princomp(data_this_window)
    load=loadings(PCA)
    appearance_this_window=load[,1] %>% data.frame 
    names(appearance_this_window)="loading"
    appearance_this_window=appearance_this_window %>% 
      arrange(desc(abs(loading))) %>% 
      head(10)
    
    count_present=appearance_this_window[factor_name,]
    count_present[is.na(count_present)] <- 0 
    count_load_appearance$count_present=count_present %>%
      map(function(x) if (!x==0) {x=1} else {x=0}) %>% 
      unlist
    count_load_appearance$count=count_load_appearance$count+
      count_load_appearance$count_present
    count_load_appearance=count_load_appearance %>% 
      select(-count_present)
    initial=initial+A
    if (stop_loop_signal==TRUE){continue=FALSE}
    if (initial>=dim(data)[1]){stop_loop_signal=TRUE}
    if (initial>dim(data)[1]){initial=dim(data)[1]}
  }
  return (list(count_load_appearance))
}
result=Rollingwindow(data_filtered,110,1)
plot_bar=result[[1]] %>% 
  filter(!count==0) %>% 
  arrange(desc(count)) %>% 
  head(20)
par(mfrow=c(1,1))
par(mar=c(5,10,4,1)+.1)
barplot(plot_bar$count,
        names.arg=rownames(plot_bar),
        horiz=TRUE,
        las=2,
        xlab="Appearance",
        main="Factor Appearance in Principal Components")




## 5-fold CV forward and backward variable selection
# Dealing with data for regression on principal components
predict_regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coef_i = coef(object, id = id)
  mat[, names(coef_i)] %*% coef_i
}
factor_for_merge=data %>% 
  filter(X>=72)  %>% 
  select(-X)  %>% 
  select(-RF)
factor_for_merge$date=map(factor_for_merge$date,
                          function(x) format(as.Date(x),"%Y-%m-%d") ) %>% unlist 
stock_returns_with_factors=
  merge(factor_for_merge,stock_trading_data_raw) %>% select(-date)
# The Cross Validation code is modified from codes on LSE moodle
set.seed(09122021)
fold_number=5
folds = sample(rep(1:fold_number, length = nrow(stock_returns_with_factors)))
# Set the max_variable as 60
max_variables=60
cv_errors =matrix(0,fold_number, max_variables)
data_for_CV=excess_return_with_pca_factors
#forward best subset selection for regression on principal components

for(k in 1:fold_number){
  fit_fwd = regsubsets(monthly_excess_return~., data=data_for_CV[folds!=k,], nvmax=max_variables, method="forward")
  print(k)
  for(i in 1:max_variables){
    pred =predict_regsubsets(fit_fwd, data_for_CV[folds==k,], id=i)
    cv_errors[k,i] =mean((data_for_CV$monthly_excess_return[folds==k]-pred)^2)
  }
}
rmse_cv_fwd_component =sqrt(apply(cv_errors,2,mean))
#backward best subset selection for regression on principal components
cv_errors =matrix(0,5, max_variables)
for(k in 1:fold_number){
  fit_fwd = regsubsets(monthly_excess_return~., data=data_for_CV[folds!=k,], nvmax=max_variables, method="backward")
  print(k)
  for(i in 1:max_variables){
    pred =predict_regsubsets(fit_fwd, data_for_CV[folds==k,], id=i)
    cv_errors[k,i] =mean((data_for_CV$monthly_excess_return[folds==k]-pred)^2)
  }
}
rmse_cv_bwd_component =sqrt(apply(cv_errors,2,mean))
## Plot of Root MSE vs model size for regression on principal components
plot(rmse_cv_fwd_component, 
     ylab="Root MSE", 
     xlab="Model Size", 
     pch=max_variables,  
     col="red",
     type="l",
     main="5-Fold Cross Validation Using \nSubset Selection on Stock Returns \n and Principal Components")
points(which.min(rmse_cv_fwd_component), 
       rmse_cv_fwd_component[which.min(rmse_cv_fwd_component)], 
       col="red", 
       cex=2, 
       pch=20)
lines(rmse_cv_bwd_component, 
      pch=max_variables, 
      col="blue", 
      type="l")
points(which.min(rmse_cv_bwd_component), 
       rmse_cv_bwd_component[which.min(rmse_cv_bwd_component)], 
       col="blue", 
       cex=2, 
       pch=20)

#CV for Factors

data_for_CV=stock_returns_with_factors
#forward best subset selection for regression on factors
for(k in 1:fold_number){
  fit_fwd = regsubsets(monthly_excess_return~., data=data_for_CV[folds!=k,], nvmax=max_variables, method="forward")
  print(k)
  for(i in 1:max_variables){
    pred =predict_regsubsets(fit_fwd, data_for_CV[folds==k,], id=i)
    cv_errors[k,i] =mean((data_for_CV$monthly_excess_return[folds==k]-pred)^2)
  }
}
rmse_cv_fwd_factor =sqrt(apply(cv_errors,2,mean))
#backward best subset selection for regression on factors
cv_errors =matrix(0,5, max_variables)
for(k in 1:fold_number){
  fit_fwd = regsubsets(monthly_excess_return~., data=data_for_CV[folds!=k,], nvmax=max_variables, method="backward")
  print(k)
  for(i in 1:max_variables){
    pred =predict_regsubsets(fit_fwd, data_for_CV[folds==k,], id=i)
    cv_errors[k,i] =mean((data_for_CV$monthly_excess_return[folds==k]-pred)^2)
  }
}
rmse_cv_bwd_factor =sqrt(apply(cv_errors,2,mean))
## Plot of Root MSE vs model size for regression on factors
lines(rmse_cv_fwd_factor, 
      pch=max_variables, 
      col="green", 
      type="l")
points(which.min(rmse_cv_fwd_factor), 
       rmse_cv_fwd_factor[which.min(rmse_cv_fwd_factor)], 
       col="green", 
       cex=2, 
       pch=20)
lines(rmse_cv_bwd_factor, 
      pch=max_variables, 
      col="purple", 
      type="l")
points(which.min(rmse_cv_bwd_factor), 
       rmse_cv_bwd_factor[which.min(rmse_cv_bwd_factor)], 
       col="purple", 
       cex=2, 
       pch=20)
legend(x = "topright",
       legend = c("Forward Components", "Backward Components","Forward Factors", "Backward Factors"),
       col = c("red", "blue","green", "purple"),
       lwd= 2)               
std_fwd_component=min(rmse_cv_fwd_component)+(rmse_cv_fwd_component) %>% var %>% sqrt
std_bwd_component=min(rmse_cv_bwd_component)+(rmse_cv_bwd_component) %>% var %>% sqrt
std_fwd_factor=min(rmse_cv_fwd_factor)+(rmse_cv_fwd_factor) %>% var %>% sqrt
std_bwd_factor=min(rmse_cv_bwd_factor)+(rmse_cv_bwd_factor) %>% var %>% sqrt
lines(rep(std_fwd_component,60,color="red"))
lines(rep(std_bwd_component,60,color="blue"))
lines(rep(std_fwd_factor,60,color="green"))
lines(rep(std_bwd_factort,60,color="purple"))
find_first_element <- function(data,interval_range)
{
  i=1
  while ((i<=length(data)) & (data[i]>interval_range))
  {
    i=i+1
  }
  return (i)
}
find_first_element(rmse_cv_fwd_component,std_fwd_component)
find_first_element(rmse_cv_bwd_component,std_bwd_component)
find_first_element(rmse_cv_fwd_factor,std_fwd_factor)
find_first_element(rmse_cv_bwd_factor,std_bwd_factor)
reg_fwd_factor=regsubsets(monthly_excess_return~.,data=stock_returns_with_factors,nvmax=16, method="forward")
reg_bwd_factor=regsubsets(monthly_excess_return~.,data=stock_returns_with_factors,nvmax=16, method="backward")
data.frame(coef(reg_fwd_factor, 14))
data.frame(coef(reg_bwd_factor, 15))
rmse_cv_fwd_factor[14]
rmse_cv_bwd_factor[15]
reg_fwd_component=regsubsets(monthly_excess_return~.,data=excess_return_with_pca_factors,nvmax=7, method="forward")
reg_bwd_component=regsubsets(monthly_excess_return~.,data=excess_return_with_pca_factors,nvmax=7, method="backward")
data.frame(coef(reg_fwd_component, 7))
data.frame(coef(reg_bwd_component, 7))
rmse_cv_bwd_component[7]
rmse_cv_fwd_component[7]

