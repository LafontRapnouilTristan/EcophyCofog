#' NotIn
#'
#' @name %notin%
#' @usage x \%notin\% y
#' @param x vector to be tested
#' @param y vectore to be tested against
#' @description create an operator being the opposite of %in%
#' @return notin operator
#' @export

`%notin%` <- function(x,y){
  x[!x %in% y]
  }

