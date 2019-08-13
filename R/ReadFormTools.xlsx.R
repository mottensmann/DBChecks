#' Format FormTools output
#'
#' @param path path
#' @param file file pattern
#' @param type file type
#' @import magrittr
#' @export
#'
ReadFormTools.xlsx <- function(path = "data", file = "webform_download", type = ".xlsx") {
  ## read raw file
  data <- readxl::read_xlsx(file.select(path, file, type),
                            trim_ws = T, skip = 3,
                            col_names = c(
                              "ID", "Name" ,"Email", "Species",
                              "Condition", "Color", "Caption", "Ring", "Photo",
                              'Long', 'Lat', "Where", "Date", "When", "Comments", "History",
                              "Data.privacy.statement", "Submitted",
                              "Last modified"),
                            col_types = c(
                              "numeric", "text", "text", "text", "text",
                              "text", "text", "text", "text", "text",
                              "text", "text", "date", "text", "text",
                              "text", "text", "date", "date"
                            ))

  ## format wingtag
data[["wingtag"]] <- NULL
for (i in 1:nrow(data)) {
  data[["wingtag"]][i] <- paste0(data[["Caption"]][i]) %>%
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


