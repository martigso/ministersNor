#' Mode
#' 
#' A function to extract the mode of a varible. Borrowed from user Ken Williams at https://stackoverflow.com/a/8189441
#' 
#' @param x A vector of class character, factor, or numeric
#' 
#' @return Returns the mode of the input variable
#' 
#' 
#' 
#' @examples 
#' getmode(sample(1:2, 1000, replace = TRUE))
#' 
#' @export
#' 

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
