library(jsonlite)
library(RCurl)
setwd("~/ons/register-proto")
ons_url <- "https://www.ons.gov.uk"

download <- function(base, url, suffix) {
  url <- paste(base,url, suffix, sep = "")
  print(url)
  raw_data <- getURL(url)
  raw_data <- gsub("\n", "", raw_data, fixed = TRUE)
  raw_data <- gsub("\t", "", raw_data, fixed = TRUE)
  raw_data <- gsub("\\*", "*", raw_data, fixed = TRUE)
  raw_data <- gsub("\\-", "-", raw_data, fixed = TRUE)
  raw_data <- gsub("\\.", ".", raw_data, fixed = TRUE)
  try(return(fromJSON(raw_data)))
  return("Error")
}

pages <- read.csv('pages.csv', stringsAsFactors = F)
pages <- pages[pages$type == "product_page",]

json <- lapply(pages$uri,function(x) download(ons_url, x , "/data"))
write(toJSON(json), "products.json")

datasets <- lapply(1:length(json), function(index) {
  try({datasets <- json[[index]]$datasets
  d <- data.frame(uri = datasets$uri, stringsAsFactors = F)
  d$product_page = json[[index]]$uri
  d$type = "dataset"
  return(d)})
  return("Error")
})
datasets <- datasets[datasets != "Error"]
datasets <- do.call("rbind", datasets)

statsBulletins <- lapply(1:length(json), function(index) {
  try({statsBulletins <- json[[index]]$statsBulletins
       d <- data.frame(uri = statsBulletins$uri, stringsAsFactors = F)
       d$product_page = json[[index]]$uri
       d$type = "bulletin"
       return(d)})
  return("Error")
})
statsBulletins <- statsBulletins[statsBulletins != "Error"]
statsBulletins <- do.call("rbind", statsBulletins)

relatedArticles <- lapply(1:length(json), function(index) {
  try({relatedArticles <- json[[index]]$relatedArticles
       d <- data.frame(uri = relatedArticles$uri, stringsAsFactors = F)
       d$product_page = json[[index]]$uri
       d$type = "article"
       return(d)})
  return("Error")
})
relatedArticles <- relatedArticles[relatedArticles != "Error"]
relatedArticles <- do.call("rbind", relatedArticles)

content <- rbind(statsBulletins, datasets, relatedArticles)
write.csv(content, "content.csv")

latestifyUri <- function(uri) {
  if(uri == "/businessindustryandtrade/business/businessinnovation/articles/londonanalysis/2015-02-13/2015-02-13") {
    return(uri)
  }
  if(grepl("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", uri)) {
    return(paste(substr(uri, 1, nchar(uri) - 10),"latest", sep=""))
  }
  if(grepl("/[0-9][0-9][0-9][0-9]$", uri)) {
    return(paste(substr(uri, 1, nchar(uri) - 4),"latest", sep=""))
  }
  return(uri)
}

# Replace dates with latest where necessary
uris <- latestifyUri(content$uri)

nullCheck <- function(x) {
  if(is.null(x)) return("")
  return(x)
}

productJson <- lapply(content$uri,function(x) download(ons_url, x , "/data"))
productJson <- productJson[productJson != "Error"]
write(toJSON(productJson), "product.json")
products <- lapply(1:length(productJson), function(index) {
        p <- productJson[[index]]
       d <- data.frame(uri = latestifyUri(p$uri), 
                       type = nullCheck(p$type),
                       title = nullCheck(p$description$title),
                       summary = nullCheck(p$description$summary),
                       metaDescription = nullCheck(p$description$metaDescription),
                       contactEmail = nullCheck(p$description$contact$email),
                       contactName = nullCheck(p$description$contact$name),
                       contactTelephone = nullCheck(p$description$contact$telephone),
                       stringsAsFactors = F)
       colnames(d) = c("uri", "type", "title", "summary", "metaDescription", "contactEmail", "contactName", "contactTelephone")
       return(d)
})
products <- do.call("rbind", products)
write.csv(products, "products.csv")

types <- unique(products$type)
for(type in types) {
  rowCount <- nrow(products[products$type == type,])
  names <- paste(type, 1:rowCount, sep="_")
  products[products$type == type,"id"] <- names
}
write.csv(products, "products.csv")

productIds <- products$id
dir.create("products")
for(id in productIds) {
  item <- products[products$id == id]
  data <- toJSON(item)
  dir.create(paste("products/", id, sep=""))
  write(data, paste("products/", id,"/data.json", sep=""))
  write(data, paste("products/", id,"/index.html", sep=""))
}
