#' Reduction
#' @description Reduit un systeme d'inequations linéaires et determine sa compatibilite
#'
#' @param coeff matrice de coefficients du système
#' @param constantes  vecteur de constantes du systeme
#' @param sup vecteur de bornes sup
#' @param inf vecteur de bornes inf
#'
#' @return retourne une liste avec la compatibilité du systeme et les formes reduit du systeme
#' @export
#' @importFrom shiny  withProgress #incProgress
#' @examples
#' #coeff=matrix(c(1,1,1,0,0,1),byrow=TRUE,ncol=2,nrow=3)
#' #sup=c(50,75,25)
#' #inf=c(12,10,15)
#' #constantes=c(12,2,3)
#' #reduction(coeff,constantes,sup,inf)
reduction<-function(coeff,constantes,sup=NA,inf=NA){
  #Gestion des na####
  if(!all(is.na(sup)==F) | !all(is.na(inf)==F)){
    inf[is.na(inf)]=-Inf
    sup[is.na(sup)]=Inf
  }
  #incProgress(0.09, detail = "Reduction systeme : Gestion des constantes additives")
  #Gestion des constantes additive####
  if(!all(constantes==0)){
    sup=sup-constantes
    inf=inf-constantes
    constantes=0
  }
  #incProgress(0.09, detail = "Reduction systeme : Gestion des lignes nulles")
  #Gestion des lignes nulles####
  presnull=which(apply(coeff,1,function(x) all(x==0)))
  if(length(presnull)>0){
    supprimer=which(inf[presnull]<=0 & sup[presnull]>=0)
    if(length(supprimer)>0){
      ifelse(ncol(coeff)==1,
             coeff<- matrix(coeff[-supprimer,],ncol = 1),
             coeff<- coeff[-supprimer,]
             )
      sup=sup[-supprimer]
      inf=inf[-supprimer]
    }
  }
  #Definition des variables####
  frontiere=(nrow(coeff)-ncol(coeff)+1):nrow(coeff)
  identite=coeff[frontiere,]
  supidentite=sup[frontiere]
  infidentite=inf[frontiere]
  infidentite[which(infidentite<0)]=0
  systeme=matrix(coeff[-frontiere,],ncol=ncol(coeff))
  supsysteme=sup[-frontiere]
  infsysteme=inf[-frontiere]
  ##Borne identite ####
  if(!is.matrix(identite)){
    identite<-matrix(identite)
  }
  # Matrice de contraintes
  contraintes <- rbind(coeff,coeff)
  # Borne des contraintes
  bornes <- c(sup,inf)
  # Orientation des contraintes
  constranints_direction  <- c(rep("<=",nrow(coeff)),rep(">=",nrow(coeff)))
  utile=c("status","objval","solution")
  # #incProgress(0.09, detail = "Reduction systeme : Bornes inférieures des paramètres")
  binf=sapply(1:ncol(identite),function(x) {#incProgress(0.09/ncol(identite), detail = paste0("Reduction systeme : Borne inférieure du paramètre ",x," sur ",ncol(identite)))
                                           lp(direction="min",
                                              objective.in = identite[x,],
                                              const.mat = contraintes,
                                              const.dir = constranints_direction,
                                              const.rhs = bornes,
                                              all.int = T)[utile]} )
  infidentite=round(unlist(binf["objval",]))
  ##incProgress(0.09, detail = "Reduction systeme : Bornes superieures des paramètres")
  bsup=sapply(1:ncol(identite),function(x){ #incProgress(0.09/ncol(identite), detail = paste0("Reduction systeme : Borne superieur du paramètre ",x," sur ",ncol(identite)))
                                            lp(direction="max",
                                              objective.in = identite[x,],
                                              const.mat = contraintes,
                                              const.dir = constranints_direction,
                                              const.rhs = bornes,
                                              all.int = T)[utile]})
  supidentite=round(unlist(bsup["objval",]))
  statut=unlist(c(binf["status",],bsup["status",]))
  ####FIN####
  condition=list(coefficient=rbind(systeme,identite),sup=rbind(matrix(supsysteme),matrix(supidentite)),inf=rbind(matrix(infsysteme),matrix(infidentite)))
  return(list(statuts=statut,systeme=systeme,infsysteme=infsysteme,supsysteme=supsysteme,identite=identite,infidentite=infidentite,supidentite=supidentite,condition=condition))
}
