#! /usr/bin/Rscript
library(AmazonCPI)
items = getData(1)
write.csv(items, paste('/home/production/AmazonCPI/Top100wCats',Sys.Date(),sep='_'))

#indivItems = getIndivItems(items[sample(1:nrow(items),500),] )

#write.csv(indivItems, paste('/home/production/AmazonCPI/Top100Detail',Sys.Date(),sep='_'))
