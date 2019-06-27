#' extremum
#' @description Permet de determiner la borne sup et inf de d'une variable
#' @param sysineq systeme d'inequation auquelle est rattache une variable
#' @param direction "min" pour borne inf ou "max" pour borne sup
#' @param numeros numeros de la variable
#'
#' @return la borne sup ou inf de la variable
#' @export
#'
#' @examples
#' #no exemple
extremum<-function(sysineq,direction,numeros){
  # Matrice de contraintes
  contraintes <- rbind(sysineq$condition$coefficient,sysineq$condition$coefficient)
  # Borne des contraintes
  bornes <- c(sysineq$condition$sup,sysineq$condition$inf)
  # Orientation des contraintes
  constranints_direction  <- c(rep("<=",nrow(sysineq$condition$coefficient)),rep(">=",nrow(sysineq$condition$coefficient)))

  born=lp(direction=direction,
          objective.in = sysineq$identite[numeros,],
          const.mat = contraintes,
          const.dir = constranints_direction,
          const.rhs = bornes,
          all.int = T)
  return(list(statut=born$status,borne=round(born$objva)))
}
