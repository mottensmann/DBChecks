#' Format FormTools output
#'
#' @param path path
#' @param file file pattern
#' @param type file type
#' @import magrittr
#' @export
#'
ReadFormTools <- function(path = "data", file = "form561", type = ".csv") {
  ## read raw file
  data <- read.csv(file.select(path, file, type),
                sep = ";", encoding = "UTF-8", header = F,
                skip = 1, col.names = c(
                  "ID", "Name" ,"Email", "Species",
                  "Condition", "Color", "Caption", "Another.Caption", "Ring",
                  "Where", 'Long', 'Lat', "Date", "When", "Comments", "History",
                  "Data.privacy.statement", "Spam.Vermeidung", "Photo", "Submitted",
                  "Last modified"),
                stringsAsFactors = F)
  ## format wingtag
data[["wingtag"]] <- NULL
for (i in 1:nrow(data)) {
  data[["wingtag"]][i] <- paste0(data[["Caption"]][i], data[["Another.Caption"]][i]) %>%
    stringr::str_remove(., "another") %>%
    toupper()
  data[["wingtag"]][i] <- paste("Metal right,", data[["Color"]][i], "wingtag", data[["wingtag"]][i])
  data[["Comments"]][i] <- ifelse(nchar(data[["Comments"]][i]) == 0, NA, data[["Comments"]][i])
}

## set up data frame sturcture
df <- data.frame(
  Date = as.POSIXct(data[["Date"]], format = "%d-%b-%y") %>%
    as.Date() - 365,
  Coords = NA,
  Where = data[["Where"]],
  Lat = data[["Lat"]],
  Long = data[["Long"]],
  ID = data[["wingtag"]],
  Ring = data[["Ring"]],
  Observer = data[["Name"]],
  Link = NA,
  Remarks = data[["Comments"]],
  Recruited = NA,
  Tags = NA,
  Mail = data[["Email"]],
  LifeHistory = data[["History"]],
  Submitted = data[["Submitted"]] %>%
    as.character())

## trim whitespaces and special characters
df <- apply(df,2,trimws) %>%
  as.data.frame()
df <- apply(df,2,gsub, pattern = "[\r\n]", replacement = "") %>%
  as.data.frame()

## export to csv file
write.csv2(x = df, file =  paste0(path, "/ResightsFormTools.csv"), quote = FALSE, row.names = FALSE)
check <- "Done"
return(check)
}


