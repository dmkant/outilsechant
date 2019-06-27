#' @title fusion
#'
#'
#' @description Une fonction équivalente à rbind mais sur une liste de fonction ou de matrice.
#'
#' @param l une liste de matrice ou df
#'
#' @return un df ou matrice résultant de la fusion de ceux contenus dans l
#'
#' @examples
#' #fusion(list(matrix(1,5,5),matrix(0,5,5)))
#'
fusion<-function(l){
  if(length(l)==1){
    return(rbind(l[[1]]))
  }
  else{
    return(rbind(fusion(l[-length(l)]),l[length(l)][[1]]))
  }
}
