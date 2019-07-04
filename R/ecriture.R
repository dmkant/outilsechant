#' Ecriture d'un nombre 
#'
#' @param nombre 
#'
#' @return une chaine de caractère
#' @export
#'
#' @examples
#' ecriture(12334)
ecriture<-function(nombre){
  options(scipen=999)
  taille<-nchar(nombre)
  if(taille<=3){return(nombre)}
  if(taille<=6){
    if(taille==4){return(paste(substr(nombre,1,1),substr(nombre,2,4)))}
    if(taille==5){
      return(paste(substr(nombre,1,2),substr(nombre,3,5)))
    }
    if(taille==6){
      return(paste(substr(nombre,1,3),substr(nombre,4,6)))
    }
  }
  if(taille<=9){
    return(paste(substr(nombre,1,1),substr(nombre,2,4),substr(nombre,5,7)))
  }
}