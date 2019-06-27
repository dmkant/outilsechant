#' methode par optimisation
#'
#' @param foncoptm vecteur de coefficient de la fonction a optimiser
#' @param faisabl resultat de la fonction faisable()
#'
#' @return un vecteur qui permet de determiner la configuration de l'echantillon
#' @export
#' @importFrom shiny  withProgress incProgress
#' @examples
#' #pas d'exemple
methodeopt<-function(foncoptm,faisabl){
  # Matrice de contraintes
  contraintes <- rbind(faisabl$reduit$condition$coefficient,faisabl$reduit$condition$coefficient)
  # Bornes des contraintes#
  bornes <- c(faisabl$reduit$condition$inf,faisabl$reduit$condition$sup)
  # Orientation des contraintes#
  nbcont=nrow(faisabl$reduit$condition$coefficient)
  constranints_direction  <- c(rep(">=",nbcont),rep("<=",nbcont))
  optimum <-  lp(direction="max",
                 objective.in = foncoptm,
                 const.mat = contraintes,
                 const.dir = constranints_direction,
                 const.rhs = bornes,
                 all.int = T)
  # increase progress
  incProgress(0.75, detail = "Construction de l'échantillon : Taille des sous-échantillons")
  return(matrix(optimum$solution))
}
