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
  
  #by Item %>%
  masterSummary = data %>% 
    mutate(azpriceNum=as.numeric(gsub('\\$','',as.character(data$azprice)))) %>%
    group_by(itemTitle) %>%
    summarize(price=azpriceNum[1],
              reviews=as.numeric(as.character(reviews[1]))) %>%
    #na.omit %>%
    summarize(avgPrice=mean(price, na.rm = T),
              sdPrice=sd(price,na.rm = T),
              numItems=length(itemTitle),
              weightedAvg=sum(price*reviews,na.rm = T)/sum(reviews,na.rm = T),
              date=ymd(date)
    )
  write.csv(masterSummary, file=gsub('Top100wCats', 'masterSummary',files[i]))
    
  #byCategory
  catSummary = data %>% 
    mutate(azpriceNum=as.numeric(gsub('\\$','',as.character(data$azprice)))) %>%
    group_by(category, itemTitle) %>%
    summarize(price=azpriceNum[1],
              reviews=as.numeric(as.character(reviews[1]))) %>%
#     na.omit %>%
    group_by(category) %>%
    summarize(avgPrice=mean(price,na.rm = T),
              sdPrice=sd(price,na.rm = T),
              numItems=length(itemTitle),
              weightedAvg=sum(price*reviews,na.rm = T)/sum(reviews,na.rm = ),
              date=ymd(date)
    ) 
  write.csv(catSummary, file=gsub('Top100wCats','categorySummary',files[i]))

if(i==1){
  masterMasterSummary=masterSummary
  masterCatSummary=catSummary
}else{
  masterMasterSummary = rbind(masterMasterSummary, masterSummary)
  masterCatSummary = rbind(masterCatSummary, catSummary)
  
}  
  
}

#write the combined summary into 1 file for each
write.csv(masterMasterSummary, '/home/production/AmazonCPI/masterSummary.csv')
write.csv(masterCatSummary, '/home/production/AmazonCPI/catSummary.csv')

