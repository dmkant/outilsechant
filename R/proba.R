#' proba
#'
#' @param poid le poid de la variable dans la fonction à optimiser
#' @param inf borne inf de la variable
#' @param sup born sup de la variable
#'
#' @return une loi de probabilite
#' @export
#'
#' @examples
#' proba(0.9,0,100)
proba<-function(poid,inf,sup){
  taille=(sup-inf+1)
  ifelse(poid>=0,poid<-poid+1,poid<-poid-1)
  a=exp((inf:sup)*poid/sqrt(taille))/taille
  a=a/sum(exp((inf:sup)*poid/sqrt(taille))/taille)
  return(a)
}
