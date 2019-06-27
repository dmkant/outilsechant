#' coupure
#'
#' @param x vecteur de nombre
#'
#' @return une liste
#' @export
#'
#' @examples
#' #pas dexemple
coupure<-function(x){
  resultat=list()
  if (length(x)==1){
    return(append(resultat,list(c(1,x))))
  }
  else{
    precedent=unlist(coupure(x[-length(x)])[length(x)-1])[2]+1
    suivant=unlist(coupure(x[-length(x)])[length(x)-1])[2]+x[length(x)]
    return(append(coupure(x[-length(x)]),list(c(precedent,suivant))))
  }
}
