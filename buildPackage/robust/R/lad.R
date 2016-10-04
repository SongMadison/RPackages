#' Calculate the Least absolute deviation (title)
#'
#' Calculate the Least absolute deviation for single covariate ("Description" paragraph)
#' @param x   single predictor variable 
#' @param y   response variable 
#' @return coefficients   intercept and slope of the fitted line
#'         fitted.values  fitted.values
#'         residuals      rediduals
#' @details (Details section)
#' @export
#' @examples 
#'  # Example
#'  area = read.csv("http://www.stat.wisc.edu/~jgillett/327-3/2/farmLandArea.csv")
#'  out <- lad(x=area$land, y=area$farm)
#'  out



lad <- function(x, y) {
    loss <- function(beta, ...){
        res <- 0
        for(i in 1:length(y)){
            res <- res + abs( y[i]-beta[1]- beta[2]*x[i] )
        }
        return (res)
    }
    lm1 <- lm(y~x)
    optimRes <- optim(par =lm1$coef , fn = loss, method = "Nelder-Mead")
    #return(optimRes)
    out<- list()
    out$coefficients <- optimRes$par
    out$fitted.values <- out$coefficients[1]+out$coefficients[2]*x
    out$residuals <- y - out$fitted.values
    class(out) <- "lad"
    return(out)
}


