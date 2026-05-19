# this function is suppose top fiz/improve column names. The data is too heterogeneous to work easily. might remove it
standardize_colnames <- function(colnames) {
  # Remove multiple dots or trailing dots
  colnames <- gsub("\\.+", "_", colnames)
  colnames <- gsub("_$", "", colnames)
  
  cleaned <- sapply(colnames, function(col) {
    # Look for year and suffix at the end
    match <- stringr::str_match(col, "(.*?)(?:_|)(19|20)[0-9]{2}(?:_|)(sum|mean)$")
    
    if (is.na(match[1, 1])) return(col)  # return original if no match
    
    prefix <- match[1, 2]
    year <- match[1, 3]
    suffix <- match[1, 4]
    
    # Remove trailing underscores from prefix
    prefix <- gsub("_$", "", prefix)
    
    paste0(prefix, "_", year, "_", suffix)
  })
  
  return(cleaned)
}
