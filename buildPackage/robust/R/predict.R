
#' predict method for lad
#'
#' predict the response at newly observed value of covariates 
#' @param ladObj   lad Object 
#' @param newX   new values of the covariate 
#' @return Ypred    predicted value at new values
#' @details  predict the response at newly observed value of covariates
#' @export
#' @examples 
#'  area = read.csv("http://www.stat.wisc.edu/~jgillett/327-3/2/farmLandArea.csv")
#'  out <- lad(x=area$land, y=area$farm)
#'  ypred <- predict(out, newX=c(10000,20000))


predict.lad <- function(ladObj, newX){
    ## assume single variate
    res <- list()
    beta <- ladObj$coefficients
    res$Ypred <- beta[1]+beta[2]*newX
    return (res)
}