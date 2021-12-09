
setwd("C:\\Users\\technoplanet\\Desktop\\LSE Projects\\ST443\\Group Project")

# Reading file
df=read.csv("final_regression_data_all.csv",header=T)

#Installing packages

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
write.csv(clean_big_data,"cleaned_stock_return_with_factors.csv")
