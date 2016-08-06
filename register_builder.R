

fluff <- function(url, replaceFields, withValues) {
  lines <- readLines(url)
  for(i in 1:length(replaceFields)) {
    replaceValue <- paste("{{", replaceFields[i], "}}", sep="")
    replaceWith <- withValues[i]
    lines <- gsub(replaceValue, replaceWith, lines, fixed = TRUE)
  }
  return(lines)
}

copyCSS <- function(registerPath) {
  dir.create(path = registerPath, showWarnings = FALSE)
  file.copy(from="templates/files", to=registerPath, 
            overwrite = TRUE, recursive = TRUE, 
            copy.mode = TRUE)  
}

buildRootRecord <- function(data, registerName, registerPath) {
  fieldList <- paste(colnames(data), collapse=", ")
  lines <- fluff("templates/template.htm", 
                 c("RegisterTitle", "RegisterLink", "RecordCount", "FieldList"),
                 c(registerName, "", nrow(data), fieldList))
  write(x = lines, file = paste(registerPath,"index.htm",sep="/"))
}

buildSingleRecordsPages <- function(data, registerName, registerPath, idField = "id") {
  ids <- data[, idField]
  
  dir.create(paste(registerPath, "record", sep="/"))
  for(id in ids) {
    item <- data[data[,idField] == id]
    
    dir.create(paste(registerPath, "record", id, sep="/"))
    
    json <- toJSON(item)
    write(json, paste(registerPath, "record", id,"data.json", sep="/"))
    
    html <- fluffSingleRecordsPage(item, registerName, 1, idField)
    write(html, paste(registerPath, "record", id,"index.html", sep="/"))
  }
}

fluffSingleRecordsPage <- function(data, registerName, row, idField = "id") {
  rowData = c()
  cols <- colnames(data)
  for(i in 1:ncol(data)) {
    newRows <- fluff(url = "templates/record/id/tablecell.htm", 
                     replaceFields = c("FieldName", "FieldValue"), withValues = c(cols[i], data[row, i]))
    print(length(newRows))
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  fullData <- fluff(url = "templates/record/id/template.htm", 
               replaceFields = c("RegisterTitle", "RegisterLink", "ItemId", "RowData"),
               withValues = c(registerName, "", data[row, idField], rowData))
  return(fullData)
}

buildRegister <- function(data, registerName, registerPath, idField = "id", detailFields = NULL) {
  copyCSS(registerPath)
  buildRootRecord(data, registerName, registerPath)
  buildSingleRecordsPages(data, registerName, registerPath, idField)
}