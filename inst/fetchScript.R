#! /usr/bin/Rscript
# an RScript to fetch the data and then compiles it to the CPI with extra information

library(dplyr)
library(lubridate)
library(magrittr)
#find non summarized files, summarize them... then put all the summaries together into some time series

files=list.files('/home/production/AmazonCPI/',pattern = 'Top100wCats*', full.names = T) 

for(i in 1:length(files)){
  data =  read.csv(files[i])
  date = strsplit(strsplit(files[i], '/')[[1]][6], '_')[[1]][2]
  
  #by Item %.%
  data %.% 
    mutate(azpriceNum=as.numeric(gsub('\\$','',as.character(data$azprice)))) %.%
    group_by(itemTitle) %.%
    summarize(price=azpriceNum[1],
              reviews=as.numeric(as.character(reviews[1]))) %>%
    na.omit %>%
    summarize(avgPrice=mean(price),
              sdPrice=sd(price),
              numItems=length(itemTitle),
              weightedAvg=sum(price*reviews)/sum(reviews),
              date=ymd(date)
    ) %>% write.csv(file=gsub('Top100wCats', 'masterSummary',files[i]))
    
  #byCategory
  data %.% 
    mutate(azpriceNum=as.numeric(gsub('\\$','',as.character(data$azprice)))) %.%
    group_by(category, itemTitle) %.%
    summarize(price=azpriceNum[1],
              reviews=as.numeric(as.character(reviews[1]))) %>%
    na.omit %>%
    group_by(category) %.%
    summarize(avgPrice=mean(price),
              sdPrice=sd(price),
              numItems=length(itemTitle),
              weightedAvg=sum(price*reviews)/sum(reviews),
              date=ymd(date)
    ) %>% write.csv(file=gsub('Top100wCats','categorySummary',files[i]))

}
