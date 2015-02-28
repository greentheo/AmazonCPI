#' Fetch the data from Amazon.com
#' @param timeout - (default=1) set the time between requests
#' @import rvest
#' @import dplyr
#' @export
getData = function(timeout=1){
  
  #highlevel Categories
  highCats = getCats('http://www.amazon.com/Best-Sellers/zgbs')

  
  #for each category get items and subcategories
  items = data.frame(itemTitle=NA, stars=NA, reviews=NA, azprice=NA, 
                     link=NA, category=NA, subCategory=NA)
  for(i in 1:nrow(highCats)){
    cat('\n getItems and subcategories for ', as.character(highCats$text[i]), '-')
    Sys.sleep(1)
    #get subCats
    subCats = getCats(as.character(highCats$link[i]))
    
    #getItems
    for(j in 1:5){
      Sys.sleep(timeout)
      gitems = try(getItems(url=paste(highCats$link[i], "#",j, sep=''), highCats$text[i], NA))
      if(class(gitems)!="try-error") 
        items = rbind(items, gitems)
      
    }
    
    #get items on subPages
    for(j in 1:nrow(subCats)){  
      cat(' ', as.character(subCats$text[j]))
      for(k in 1:5){
        Sys.sleep(timeout)
        gitems=try(getItems(url=paste(subCats$link[j], "#",k, sep=''), 
                            highCats$text[i], subCats$text[j]))
        if(class(gitems)!="try-error")
          items = rbind(items, gitems)
      }      
    }
  }
  return(items[-1,])
}

#' to get categories
#' @param url, the url of the categories to retrieve
#' @export
getCats = function(url){
  cats = html(url)
  categories = cats %>%
    html_nodes(css = '#zg_browseRoot ul li a') 
  categoriesDF = data.frame(text=html_text(categories), link=html_attr(categories, 'href'))
}

#' to get individual items fromt he "getItems" function
#' @param items, the items to get detailed info about
#' @param timeout, the amouunt of time to wait between requests
#' @export
getIndivItems = function(items, timeout=1){
  uniqueItems = items %.%
    group_by(itemTitle) %.%
    summarize(url=link[1])
  
  indivItems = data.frame(uniqueItems, fullTitle=NA,
                          listPrice=NA,
                          azPrice=NA,
                          features=NA,
                          reviews=NA
  )
  
  
  for(i in 1:nrow(uniqueItems)){
    Sys.sleep(timeout)
    item = try(getItem(indivItems$url[i]))
    if(class(item)!="try-error"){
      if(length(item)==length(3:ncol(indivItems))){
        indivItems[i,3:ncol(indivItems)] = item
      }
    }
  }
  
  return(indivItems)
}

#' get items
#' @param url the url to fetch the items from 
#' @param category labelling
#' @param subCategory labeling
#' @export
getItems = function(url, category, subCategory){
  items = html(url)
  objs = items %>%
    html_nodes(css = '.zg_itemImmersion') 
  
  reviews=html_text(html_nodes(objs, css='.acr-popover+ a'))
  if(length(reviews)!=20) reviews=NA
  stars=html_text(html_nodes(objs, css='.swSprite span'))
  if(length(stars)!=20) stars=NA
  price=html_text(html_nodes(objs, css='.zg_price .price'))
  if(length(price)!=20) price=NA
  
  objs = data.frame(
    itemTitle=html_text(html_nodes(objs, css='.zg_title a')), 
    stars=stars,
    reviews=reviews,
    azprice=price,
    link=gsub('\n','',html_attr(html_nodes(objs, css='.zg_title a'), 'href')),
    category=category,
    subCategory=subCategory
  ) 
  
}

#' get a single item
#' @param url, get the information for a single item/page
#' @export
getItem = function(url){
  item = html(url)
  fullTitle=html_text(html_nodes(item, css='#productTitle'))
  listPrice=html_text(html_nodes(item, css='.a-text-strike'))
  azPrice=html_text(html_nodes(item, css='#priceblock_ourprice'))
  features=html_text(html_nodes(item, css='#feature-bullets'))
  reviews=html_text(html_nodes(item, css='#revMHLContainer'))
  
 return(data.frame(fullTitle=fullTitle,
                   listPrice=listPrice,
                   azPrice=azPrice,
                   features=features,
                   reviews=reviews
                  ))
  
}