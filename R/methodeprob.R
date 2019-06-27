#' methode probabiliste
#' @description configure l'echantillion avec une approche probabiliste
#' @param foncoptm coefficient de la fonction a optimiser
#' @param sysineq systeme d'inequation lineaire
#' @param R vecteur de parametre
#'
#' @return returne un vecteur de parametre avec des valeurs
#' @export
#' @importFrom shiny  withProgress incProgress
#' @examples
#' #no exemple
methodeprob<-function(foncoptm,sysineq,R){
  i<-1
  n<-length(foncoptm)
  repeat{
    incProgress(0.75/n, detail = paste0("Affectation d'une valeure au paramètre ",i," sur ",n))
    i<-i+1
    numeros=which(foncoptm==max(foncoptm,na.rm =T))
    numeros2=which(foncoptm[!is.na(foncoptm)]==max(foncoptm,na.rm =T ))
    if (length(numeros)>1){
      al=sample(numeros,1)
      numeros2=numeros2[which(numeros==al)]
      numeros=al
    }
    if(!is.matrix(sysineq$identite)){
      sysineq$identite<-matrix(sysineq$identite)
    }
    binf=extremum(sysineq,"min",numeros2)
    #print(binf$statut)
    bsup=extremum(sysineq,"max",numeros2)
    if(binf$statut!=0 | bsup$statut!=0){
      print("changement de methode")
      break()
      return()
    }
    #print(bsup$statut)
    ifelse(binf$borne==bsup$borne,valeur<-binf$borne,valeur<-sample(binf$borne:bsup$borne,1,F,prob = proba(foncoptm[numeros],binf$borne,bsup$borne)))
    foncoptm[numeros]=NA
    R[numeros,1]=valeur
    if(!all(is.na(foncoptm))){
      sysineq=reduire(numeros2,valeur,sysineq)
    }
    else{
      break()
    }
  }
  return(R)
}
