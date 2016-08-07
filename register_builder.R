
source("fluff.R")
source("register_index_page_builder.R")

}

buildRegister <- function(data, registerName, registerPath, detailFields, assetPath = "templates/files", idField = "id", linkFields = F) {
  copyAssets(assetPath, registerPath)
  buildRootRecord(data, registerName, registerPath)
  buildRecordsIndexPages(data, registerName, registerPath, detailFields, idField, linkFields)
  buildSingleRecordsPages(data, registerName, registerPath, idField, linkFields)
}

# Create the root register page
# Example: https://country.register.gov.uk/
buildRootRecord <- function(data, registerName, registerPath) {
  fieldList <- paste(colnames(data), collapse=", ")
  lines <- fluff("templates/template.htm", 
                 c("RegisterTitle", "RegisterLink", "RecordCount", "FieldList"),
                 c(registerName, "", nrow(data), fieldList))
  write(x = lines, file = paste(registerPath,"index.htm",sep="/"))
}

# Create the details page for individual register items
# Example: https://country.register.gov.uk/record/VA
buildSingleRecordsPages <- function(data, registerName, registerPath,  idField, linkFields) {
  ids <- data[, idField]
  
  dir.create(paste(registerPath, "record", sep="/"))
  for(id in ids) {
    item <- data[data[,idField] == id, ]
    
    dir.create(paste(registerPath, "record", id, sep="/"))
    
    json <- toJSON(item)
    write(json, paste(registerPath, "record", id,"data.json", sep="/"))
    
    html <- fluffSingleRecordsPage(item, registerName, 1, idField, linkFields)
    write(html, paste(registerPath, "record", id,"index.html", sep="/"))
  }
}

fluffSingleRecordsPage <- function(data, registerName, row, idField, linkFields) {
  rowData = c()
  cols <- colnames(data)
  for(i in 1:ncol(data)) {
    
    if(linkFields) {
      link <- paste("../../../fields/record/",cols[i], sep="")
    } else {
      link <- "#"
    }
    
    newRows <- fluff(url = "templates/record/id/tablecell.htm", 
                     replaceFields = c("FieldName", "FieldValue", "FieldLink"), withValues = c(cols[i], data[row, i], link))
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  fullData <- fluff(url = "templates/record/id/template.htm", 
               replaceFields = c("RegisterTitle", "RegisterLink", "ItemId", "RowData"),
               withValues = c(registerName, "", data[row, idField], rowData))
  return(fullData)
}


