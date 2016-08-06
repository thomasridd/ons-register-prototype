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
write(toJSON(t2), "t1.json")

# Fix up the t1 list
t1full <- lapply(1:length(t2), function(index) c(t2[[index]]$uri, t2[[index]]$type, t2[[index]]$description$title,t2[[index]]$description$summary))
t1full <- as.data.frame(t1full, row.names = c("uri", "type", "description", "summary"), stringsAsFactors = F)
t1full <- as.data.frame(t(t1full), stringsAsFactors = F)
rownames(t1full) <- t1full$uri
pages <- t1full
pages[,"parent"] <- ""
pages[,"start-date"] <- ""
pages[,"end-date"] <- ""

rm(t1full)

# Fix up the t2 list
t2list <- lapply(t1$uri,function(x) download(ons_url, x , "/data"))
t2pages <- lapply(1:length(t2list), function(index) {
    sections <- t2list[[index]]$sections
    d <- data.frame(uri = sections$uri, stringsAsFactors = F)
    d$parent = t2list[[index]]$uri
    d
  })
t2pages <- do.call("rbind", t2pages)
t2json <- lapply(t2pages$uri, function(x) download(ons_url, x , "/data"))
t2json <- t2json[t2json != "Error"]
t2full <- lapply(1:length(t2json), function(index) c(t2json[[index]]$uri, t2json[[index]]$type, t2json[[index]]$description$title, t2json[[index]]$description$summary))
t2full <- as.data.frame(t2full, row.names = c("uri", "type", "description", "summary"), stringsAsFactors = F)
t2full <- as.data.frame(t(t2full), stringsAsFactors = F)
rownames(t2full) <- t2full$uri
t2 <- merge(t2full, t2pages)
t2[,"start-date"] <- ""
t2[,"end-date"] <- ""
rm(t2full)
rm(t2pages)

pages <- rbind(pages, t2)

write.csv(pages, "pages.csv")
write(toJSON(t2json), "t2.json")

t3pages <- lapply(1:length(t2json), function(index) {
  try({sections <- t2json[[index]]$sections
  d <- data.frame(uri = sections$uri, stringsAsFactors = F)
  d$parent = t2json[[index]]$uri
  return(d)})
  return("error")
})
t3pages <- t3pages[t3pages != "error"]
t3pages <- do.call("rbind", t3pages)

t3json <- lapply(t3pages$uri, function(x) download(ons_url, x , "/data"))
write(toJSON(t3json), "t3.json")
t3json <- t3json[t3json != "Error"]
t3full <- lapply(1:length(t3json), function(index) c(t3json[[index]]$uri, t3json[[index]]$type, t3json[[index]]$description$title, t3json[[index]]$description$summary))
t3full <- as.data.frame(t3full, row.names = c("uri", "type", "description", "summary"), stringsAsFactors = F)
t3full <- as.data.frame(t(t3full), stringsAsFactors = F)
t3 <- merge(t3full, t3pages)
t3[,"start-date"] <- ""
t3[,"end-date"] <- ""
rm(t3full)
rm(t3pages)

pages <- rbind(pages, t3)
write.csv(pages, "pages.csv")


