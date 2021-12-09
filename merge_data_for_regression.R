library(dplyr)
library(purrr)
library(tidyr)
library(psych)
merge_by_date <- function(y,x)
{
  data=merge(y,x,by="date")
}
setwd("C:\\Users\\technoplanet\\Desktop\\LSE Projects\\ST443\\Group Project")
factor_returns=read.csv("factor_returns.csv")
setwd("C:\\Users\\technoplanet\\Desktop\\LSE Projects\\ST443\\Group Project\\stock_monthly_data")
files=dir()
file_read=read.csv(files[1])
total_length=0
all_merged_return_with_factors=merge_by_date(file_read,factor_returns %>% select(-X))
for (i in 1:length(files))
{
  if (i>=2)
  {
    print(i)
    stock_data_now=read.csv(files[i])
    total_length=total_length+dim(stock_data_now)[1]
    if (!dim(stock_data_now)[2]==1)
    {
      all_merged_return_with_factors=rbind(all_merged_return_with_factors,merge_by_date(stock_data_now,factor_returns %>% select(-X)))
    }
    
  }
}

setwd("D:\\")
write.csv(all_merged_return_with_factors,"final_regression_data_all.csv")
dim(all_merged_return_with_factors)
total_length
