################################################################################
#
#' expandMultipleChoice
#'
#' Function to expand response to a more than one answer multiple choice
#' question coded as a concatenated string (fix to CSPro multiple choice
#' questions)
#'
#' @param data A data.frame containing the vector data that requires expansion
#' @param x Name of variable in 'data' containing vector needing expansion
#' @param values Vector of string values used to create concatenated string
#'     response
#' @param pattern Pattern used to separate values in the concatenated string.
#'     Default is "" for concatenated strings with no separator.
#' @param prefix Prefix to names of newly created variables
#' @param labels Vector of names to use for columns of resulting data.frame.
#'     If not specified, columns are named using 'values'
#' @param sep to separate 'prefix' from 'labels' in the names of newly
#'     created variables
#'
#' @return A data.frame containing the newly created variables expanded from
#'     input data
#'
#' @examples
#'
#' #
#'
#' @export
#'
#
################################################################################

expandMultipleChoice <- function(data, x, values, pattern = "",
                                 prefix = x, labels = values, sep = ".") {
  labels <- paste(prefix, labels, sep = sep)
  temp <- stringr::str_split(data[ , x], pattern = pattern)
  result <- NULL
  for(i in 1:length(temp)) {
    for(j in values) {
      result <- c(result, ifelse(j %in% temp[[i]], 1, 2))
    }
  }
  result <- data.frame(matrix(result, ncol = length(values),
                              nrow = length(temp),
                              byrow = TRUE))
  names(result) <- labels
  return(result)
}
