###############################################################################
#
#' hash
#'
#' Simple hash function
#'
#' @param x A character vector to hash
#' @param A Default value is (sqrt(5) - 1) / 2. See Donald Knuth, 'The Art of
#'     Computer Programming', 1968
#' @param M Hash table size (limits the range of hash values)
#' @param skip If TRUE then pick every second byte (i.e. drop the UTF code page)
#' @param N Modular scaling for character codes
#'
#' @return A numeric vector of hash values
#'
#' @examples
#'
#' #
#'
#' @export
#
###############################################################################

hash <- function(x, A = 0.618034, M = 1E6, skip = TRUE, N = 11) {
  frac <- function(x) {  ## Function to get fractional part
    return(abs(x - trunc(x)))
  }
  result <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)) {
    y <- x[i]
    y <- as.numeric(charToRaw(y))   ## Convert string to a vector of raw bytes (as base-10 integers)
    y <- y[y != 32]                 ## Remove spaces
    if(skip) {
      y <- y[seq(0, length(y), 2)]  ## Drop the UTF "code page" by taking every second byte
    }
    y <- y[1:16] + 1:16             ## Generate a hash code from first 16 characters and character position
    y <- (y %% N) + 0.1             ## Hash each character code by the division method
    y <- prod(y, na.rm = TRUE)      ## Product of hashed character codes
    y <- floor(M * frac(y * A))     ## Hash product by the multiplication method
    result[i] <- y
  }
  return(result)
}
