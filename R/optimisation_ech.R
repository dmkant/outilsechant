#' Optimisation de la construction des echantillons avec la méthode à deux niveaux
#'
#' @param l resultat de la fonction optimisation_f()
#' @param methode TRUE pour l'approche probabiliste et FALSE pour l'approche par optimisation
#' @return un echantillon
#' @export
#'
#' @examples
#' iris$couleur=sample(c("rose","blanc"),150,replace = TRUE)
#' iris$risque<-sample(seq(0,1,0.01),nrow(iris),TRUE)
#' iris$tauxrep<-sample(seq(0,1,0.01),nrow(iris),TRUE)
#' constr=data.frame(variables=c("Species","Species","Species","couleur","couleur"),
#' modalites=c("setosa","versicolor","virginica","rose","blanc"),objectifs=c(25,25,25,35,40))
#' l=optimisation_f("couleur",iris,constr,75)
#' optimisation_ech(l,TRUE)
optimisation_ech<-function(l,methode){
  r=mapply(constrech,faisabl=l$faisabl,data=l$data,constr=l$constr,methode=methode,SIMPLIFY = T)
  return(fusion(r["echantillon",]))
}
