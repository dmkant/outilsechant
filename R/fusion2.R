#' fusion2
#' @description fusionne les matrices ou df d'une liste à la manière de cbind
#'
#' @param l liste de df ou matrice
#'
#' @return la fusion en colone de la liste
#' @export
#'
#' @examples
#' fusion2(list(matrix(0,5,5),matrix(1,5,5)))
fusion2<-function(l){
  if(length(l)==1){
    return(cbind(l[[1]]))
  }
  else{
    return(cbind(fusion2(l[-length(l)]),l[length(l)][[1]]))
  }
}
