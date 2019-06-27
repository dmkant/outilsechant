#' multiplication
#' @description  effectue une multiplication
#' @param m veceur de nombre
#' @param l vecteur de nombre
#'
#' @return un vecteur
#' @export
#'
#' @examples
#' #NOt exemple
multiplication<-function(m,l){
  unlist(lapply(m,function(x) x*l))
}
