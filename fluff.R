# Fluff - Templating function 
# 
# It's a long way from Mustache or Handlebars
fluff <- function(url, replaceFields, withValues) {
  lines <- readLines(url,encoding = "UTF-8")
  lines <- iconv(lines, "UTF-8", "UTF-8",sub='') 
  
  for(i in 1:length(replaceFields)) {
    replaceValue <- paste("{{", replaceFields[i], "}}", sep="")
    replaceWith <- withValues[i]
    
    lines <- gsub(replaceValue, replaceWith, lines, fixed = TRUE)
  }
  return(lines)
}

# Copy asset files to the root of the register
copyAssets <- function(assetPath, registerPath) {
  dir.create(path = registerPath, showWarnings = FALSE)
  file.copy(from=assetPath, to=registerPath, 
            overwrite = TRUE, recursive = TRUE, 
            copy.mode = TRUE)  
}

