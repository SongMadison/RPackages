
#' coef method for lad
#'
#' return the named coeffcients vector
#' @param ladObj   lad Object 
#' @return coefficients    intercept and slope
#' @details return the named coeffcients vector
#' @export
#' @examples 
#'  area = read.csv("http://www.stat.wisc.edu/~jgillett/327-3/2/farmLandArea.csv")
#'  out <- lad(x=area$land, y=area$farm)
#'  coef(out)
#'  plot(x=area$land, y=area$farm)
#'  abline(coef(out))


coef.lad <- function(ladObj){
    coefficients <- ladObj$coefficients 
    return(coefficients)
}
