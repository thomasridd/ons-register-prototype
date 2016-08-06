library(jsonlite)
library(RCurl)
setwd("~/ons/register-proto")
ons_url <- "https://www.ons.gov.uk"

download <- function(base, url, suffix) {
  url <- paste(base,url, suffix, sep = "")
  print(url)
  raw_data <- getURL(url)
  try(return(fromJSON(raw_data)))
  return("Error")
}


t1 <- read.csv("t1.csv", stringsAsFactors = F)
t1 <- tail(t1, -4)
t2 <- lapply(t1$uri,function(x) download(ons_url, x , "/data"))

# Fix up the t1 list
t1full <- lapply(1:length(t2), function(index) c(t2[[index]]$uri, t2[[index]]$description$title,t2[[index]]$description$summary))
t1full <- as.data.frame(t1full, row.names = c("uri", "description", "summary"), stringsAsFactors = F)
t1full <- as.data.frame(t(t1full), stringsAsFactors = F)
rownames(t1full) <- t1full$uri
t1full[,"start-date"] <- ""
t1full[,"end-date"] <- ""
t1 <- t1full
rm(t1full)

# Fix up the t2 list
t2list <- lapply(t1$uri,function(x) download(ons_url, x , "/data"))
t2pages <- lapply(1:length(t2), function(index) {
    sections <- t2[[index]]$sections
    d <- data.frame(uri = sections$uri, stringsAsFactors = F)
    d$parent = t2[[index]]$uri
    d
  })
t2pages <- do.call("rbind", t2pages)
t2json <- lapply(t2pages$uri, function(x) download(ons_url, x , "/data"))
t2json <- t2json[t2json != "Error"]
t2full <- lapply(1:length(t2json), function(index) c(t2json[[index]]$uri, t2json[[index]]$description$title, t2json[[index]]$description$summary))
t2full <- as.data.frame(t2full, row.names = c("uri", "description", "summary"), stringsAsFactors = F)
t2full <- as.data.frame(t(t2full), stringsAsFactors = F)
rownames(t2full) <- t2full$uri
t2 <- merge(t2full, t2pages)
t2full[,"start-date"] <- ""
t2full[,"end-date"] <- ""
rm(t2full)
rm(t2pages)

