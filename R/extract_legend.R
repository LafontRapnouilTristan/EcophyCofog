#' xtract_legend
#'
#' @param my_ggp a ggplot object to which you want to get the legend
#'
#' @return a legend object that can be added to a ggplot
#' @export
#' @import ggplot2
xtract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <-
    which(sapply(step1$grobs, function(x)
      x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
