library("ggpubr")
library(tidyverse)

#Load the dataset into a dataframe.
homedf<-read.csv("train.csv")
#The dataset is loaded into homedf

#Column Names in dataset
names(homedf)

#Number of Rows and Columns
nrow(homedf)
ncol(homedf)
#The dataset has 1460 observations and 81 variables.

dim(homedf)
#Hence the dataset is of the dimensions 1460X81.

#first 5 entries
head(homedf,5)

#Most expensive houses
head(homedf[order(-homedf$SalePrice),]$SalePrice)

#Least expensive houses
head(homedf[order(homedf$SalePrice),]$SalePrice)

#The prices of the houses in the dataset range from $34900 to $755000

#No of missing values in homedf
df_train_na <- as.data.frame(sapply(homedf, function(x) sum(is.na(x)))) %>%
  rename('na_values'='sapply(homedf, function(x) sum(is.na(x)))') %>% arrange(desc(na_values)) %>%
  mutate('na_percentage' = na_values/1460 *100 ) %>% filter(na_values >0)
df_train_na
#The variables where the NA values are higher than 15% are not going to be used.

#correlation scatter plot of total rooms and final sale price.
ggscatter(homedf, x = "TotRmsAbvGrd", y = "SalePrice", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
          
#correlation scatter plot of total living area and final sale price.
ggscatter(homedf, x = "GrLivArea", y = "SalePrice", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

#Structure of the dataset
str(homedf)

#Summary of the dataset 
summary(homedf)
