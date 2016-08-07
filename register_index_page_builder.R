source("fluff.R")

buildRecordsIndexPages <- function(data, registerName, registerPath, detailFields, idField, linkFields, recordsPerPage = 100) {
  pageMax = floor((nrow(data) - 1) / recordsPerPage) + 1
  dir.create(paste(registerPath, "records", sep="/"))
  for(i in 1:pageMax) {
    html <- buildRecordsIndexPage(data, registerName, registerPath, detailFields, i, idField, linkFields, recordsPerPage)
    if(i == 1) {
      write(html, paste(registerPath, "records/index.htm", sep="/"))
    } else {
      write(html, paste(registerPath, paste("records/page", i, ".htm", sep=""), sep="/"))
    }
  }
}
buildRecordsIndexPage <- function(data, registerName, registerPath, detailFields, pageNo, idField, linkFields, recordsPerPage = 100) {
  RegisterTitle <- registerName
  RegisterLink <- "#"
  MinRecord <- (recordsPerPage * pageNo) + 1 - 100
  MaxRecord <- min(nrow(data), MinRecord + recordsPerPage - 1)
  TotalRecords <- nrow(data)
  
  HeaderData <- buildRecordsIndexPageHeaders(detailFields, linkFields)
  RowData <- buildRecordsIndexPageRows(data, detailFields, MinRecord, MaxRecord, idField)
  
  PageNumber <- pageNo
  PageTotal <- floor((TotalRecords - 1) / recordsPerPage) + 1
  
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
buildRecordsIndexPageHeaders <- function(detailFields, linkFields) {
  rowData = c()
  
  
  for(detail in detailFields) {
    
    if(linkFields) {
      link <- paste("../../fields/record/",detail, sep="")
    } else {
      link <- "#"
    }
    
    newRows <- fluff("templates/records/headercell.htm", replaceFields = c("FieldName","FieldLink"), withValues = c(detail, link))
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