################################################################################
#
#' makeTable
#'
#' Make separate tables in long format
#'
#' @param data A data.frame containing data to be worked with
#' @param coreColumns Metadata / identifying data
#' @param columns Columns (without suffixes) to be included
#' @param number Number of grouped columns (used to create column suffixes)
#' @param padSuffix Logical. Do we add suffixes with "_0n" rather than "_n".
#' @param namesResult Column names in result
#' @param na.check Column number to use for checking NA values; default is 1.
#'
#' @return A data.frame in long format.
#'
#' @examples
#'
#' #
#'
#' @export
#'
#
################################################################################

makeTable <- function(data, coreColumns, columns, number, padSuffix,
                      namesResult, na.check = 1) {
  result <- data.frame()
  for(i in 1:number) {
    ## Add suffix to column names
    columnSuffix <- paste("_", i, sep = "")
    ## Special case for HH roster ...
    if(padSuffix) {
      columnSuffix <- ifelse(i < 10, paste("_0", i, sep = ""), columnSuffix)
    }
    ## Get the data
    workingColumns <- paste(columns, columnSuffix, sep = "")
    temp <- data[,c(coreColumns, workingColumns)]
    ## Remove empty rows (based NA in the first two columns)
    temp <- temp[!is.na(temp[[workingColumns[na.check]]]), ]
    ## Column names without number suffix
    names(temp) <- c(coreColumns, columns)
    ## Concatenate
    result <- rbind(result, temp)
  }
  result <- result[with(result, order(id)), ]
  row.names(result) <- 1:nrow(result)
  names(result) <- namesResult
  return(result)
}
