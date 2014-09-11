#' Pad with Leading Zeros
#'
#' Formats a character representation of a number and pads it with leading 
#'zeros to a specified number of characters.
#'
#' @param x a vector of the data to be padded with leading zeros. Missing values are permitted
#'and result in missing values in the corresponding positions in the output.
#' @param padTo a number indicating the minimum number of characters in the output.
#' @keywords manip
#' @return A character vector of the data in \code{x} with leading zeros.
#' @export
#' @examples
#' pCode <- 10
#' correctPCode <- zeroPad(pCode,5)
#' pCodes <- c("100","1000","0","12345","1565465465465465")
#' correctPCodes <- zeroPad(pCodes,5)
zeroPad <- function(x, padTo=1) {
	x <- as.character(x)
	misses <- is.na(x)
  numDigits <- nchar(x)
  padding <- padTo-numDigits
  padingZeros <- sapply(padding[padding > 0], function(y) paste(rep("0",y),collapse="",sep=""))
  x[padding > 0] <- paste(padingZeros,x[padding > 0],sep="")
	x[misses] <- NA
  return(x)
}
