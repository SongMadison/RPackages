#' print method
#'
#' writes the named coeffcients vector
#' @param ladObj   lad Object 
#' @return NULL    nothing
#' @details writes the named coeffcients vector
#' @export
#' @examples 
#' 
#'  area = read.csv("http://www.stat.wisc.edu/~jgillett/327-3/2/farmLandArea.csv")
#'  out <- lad(x=area$land, y=area$farm)
#'  print(out)

print.lad <- function(ladObj){
    print(ladObj$coefficients)
}
