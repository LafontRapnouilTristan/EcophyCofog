#' %notin%
#' @description inverse of the %in% operator

#' @rdname aliases
#' @usage NULL
#' @export
is_in <- `%in%`

#' @rdname aliases
#' @usage NULL
#' @export
not_in <- `%in%`

`%notin%` <- function(){
  a <-`%in%`
  Negate(a)
  }
