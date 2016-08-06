

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
    item <- data[data[,idField] == id, ]
    
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
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  fullData <- fluff(url = "templates/record/id/template.htm", 
               replaceFields = c("RegisterTitle", "RegisterLink", "ItemId", "RowData"),
               withValues = c(registerName, "", data[row, idField], rowData))
  return(fullData)
}

buildRecordsIndexPages <- function(data, registerName, registerPath, detailFields, idField, recordsPerPage = 100) {
  pageMax = ((nrow(data) - 1) %% recordsPerPage) + 1
  dir.create(paste(registerPath, "records", sep="/"))
  for(i in 1:pageMax) {
    html <- buildRecordsIndexPage(data, registerName, registerPath, detailFields, i, idField, recordsPerPage)
    if(i == 1) {
      write(html, paste(registerPath, "records/index.htm", sep="/"))
    } else {
      write(html, paste(registerPath, paste("records/page", i, ".htm", sep=""), sep="/"))
    }
  }
}
buildRecordsIndexPage <- function(data, registerName, registerPath, detailFields, pageNo, idField, recordsPerPage = 100) {
  RegisterTitle <- registerName
  RegisterLink <- "#"
  MinRecord <- (recordsPerPage * pageNo) + 1 - 100
  MaxRecord <- min(nrow(data), MinRecord + recordsPerPage - 1)
  TotalRecords <- nrow(data)
  
  HeaderData <- buildRecordsIndexPageHeaders(detailFields)
  RowData <- buildRecordsIndexPageRows(data, detailFields, MinRecord, MaxRecord, idField)
  
  PageNumber <- pageNo
  PageTotal <- ((TotalRecords - 1) %% recordsPerPage) + 1
  
  if(pageNo == 1) {
    PreviousVisibility <- "hidden"
  } else {
    PreviousVisibility <- ""
  }
  PreviousLink <- "index.htm"
  if(pageNo > 2) { PreviousLink <- paste("page", pageNo - 1, ".htm", sep = "") }
  
  if(pageNo >= PageTotal) {
    NextVisibility <- "hidden"
  } else {
    NextVisibility <- ""
  }
  NextLink <- paste("page", pageNo + 1, ".htm", sep = "")
  
  replaceFields <- c("RegisterTitle","RegisterLink","MinRecord","MaxRecord","TotalRecords",
                     "HeaderData","RowData","PageNumber","PageTotal",
                     "PreviousVisibility","PreviousLink","NextVisibility","NextLink")
  replaceWith <- c(RegisterTitle,RegisterLink,MinRecord,MaxRecord,TotalRecords,
                   HeaderData,RowData,PageNumber,PageTotal,
                   PreviousVisibility,PreviousLink,NextVisibility,NextLink)
  
  html <- fluff(url = "templates/records/template.htm", 
                replaceFields = replaceFields, withValues = replaceWith)
  
  return(html)
}
buildRecordsIndexPageHeaders <- function(detailFields) {
  rowData = c()
  for(detail in detailFields) {
    newRows <- fluff("templates/records/headercell.htm", replaceFields = c("FieldName"), withValues = c(detail))
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  return(rowData)
}

buildRecordsIndexPageRows <- function(data, detailFields, min, max, idField){
  rowData = c()
  fields <- detailFields[detailFields != idField]
  for(i in min:max) {
    values <- c(buildRecordsIndexPageRow(data, fields, i), data[i, idField])
    newRows <- fluff("templates/records/tablerow.htm", 
                     replaceFields = c("ItemCells", "IdValue"), 
                     withValues = values)
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  return(rowData)
}

buildRecordsIndexPageRow <- function(data, detailFields, row) {
  rowData = c()
  for(i in 1:length(detailFields)) {
    newRows <- fluff("templates/records/tablecell.htm", 
                     replaceFields = c("ItemValue"), withValues = c(data[row,detailFields[i]]))
    rowData <- c(rowData,newRows)
  }
  rowData <- paste(rowData, collapse = " ")
  return(rowData)
}

buildRegister <- function(data, registerName, registerPath, detailFields, idField = "id") {
  copyCSS(registerPath)
  buildRootRecord(data, registerName, registerPath)
  buildRecordsIndexPages(data, registerName, registerPath, detailFields, idField)
  buildSingleRecordsPages(data, registerName, registerPath, idField)
}
