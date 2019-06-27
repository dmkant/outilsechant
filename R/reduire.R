#' reduire
#' @description permet de reduire le systeme d'inequation aprs l attribution d'une valeur à une variable
#' @param numeros numeros de la variable a qui on vient d'attribuer une valeur
#' @param valeur valeur qu'on attribue à la variable
#' @param sysineq le systeme d'inequation associe
#'
#' @return un systeme d'inequation reduit
#' @export
#'
#' @examples
#' #no exemple
reduire<-function(numeros,valeur,sysineq){
  changement=sysineq$condition$coefficient[,numeros]
  nbpara=ncol(sysineq$condition$coefficient)
  numeros2=nrow(sysineq$condition$coefficient)-nbpara+numeros

  #Ajustement des bornes
  sysineq$condition$inf=mapply(function(coeff,borne) borne-coeff*valeur,coeff=changement,borne=sysineq$condition$inf)
  sysineq$condition$sup=mapply(function(coeff,borne) borne-coeff*valeur,coeff=changement,borne=sysineq$condition$sup)

  #Suppression ligne et colone du parametre
  sysineq$condition$coefficient=matrix(sysineq$condition$coefficient[,-numeros],ncol=nbpara-1,byrow = F)
  sysineq$condition$coefficient=matrix(sysineq$condition$coefficient[-numeros2,],ncol=nbpara-1,byrow = F)
  sysineq$condition$inf=sysineq$condition$inf[-numeros2]
  sysineq$condition$sup=sysineq$condition$sup[-numeros2]

  #Ajustement borne indentite
  frontiere=(nrow(sysineq$condition$coefficient)-ncol(sysineq$condition$coefficient)+1):nrow(sysineq$condition$coefficient)
  sysineq$identite=sysineq$condition$coefficient[frontiere,]
  sysineq$supidentite=sysineq$condition$sup[frontiere]
  sysineq$infidentite=sysineq$condition$inf[frontiere]
  sysineq$systeme=matrix(sysineq$condition$coefficient[-frontiere,],ncol=ncol(sysineq$condition$coefficient))
  sysineq$supsysteme=sysineq$condition$sup[-frontiere]
  sysineq$infsysteme=sysineq$condition$inf[-frontiere]
  return(sysineq)
}
