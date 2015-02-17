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
    cat('getItems and subcategories for ', highCats$text[i])
    Sys.sleep(1)
    #get subCats
    subCats = getCats(as.character(highCats$link[i]))
    
    #getItems
    for(j in 1:5){
      Sys.sleep(timeout)
      items = rbind(items, 
                    getItems(url=paste(highCats$link[i], "#",j, sep=''), highCats$text[i], NA))
    }
    
    #get items on subPages
    for(j in 1:nrow(subCats)){  
      for(k in 1:5){
        Sys.sleep(timeout)
        items = rbind(items, getItems(url=paste(subCats$link[j], "#",k, sep=''), 
                                      highCats$text[i], subCats$text[j]))
      }      
    }
  }
  return(items)
}

getCats = function(url){
  cats = html(url)
  categories = cats %>%
    html_nodes(css = '#zg_browseRoot ul li a') 
  categoriesDF = data.frame(text=html_text(categories), link=html_attr(categories, 'href'))
}

getItems = function(url, category, subCategory){
  items = html(url)
  objs = items %>%
    html_nodes(css = '.zg_itemImmersion') 
  
  reviews=html_text(html_nodes(objs, css='.acr-popover+ a'))
  if(length(reviews)!=20) reviews=NA
  stars=html_text(html_nodes(objs, css='.swSprite span'))
  if(length(stars)!=20) stars=NA
  
  objs = data.frame(
    itemTitle=html_text(html_nodes(objs, css='.zg_title a')), 
    stars=stars,
    reviews=reviews,
    azprice=html_text(html_nodes(objs, css='.zg_price .price')),
    link=gsub('\n','',html_attr(html_nodes(objs, css='.zg_title a'), 'href')),
    category=category,
    subCategory=subCategory
  ) 
  
}