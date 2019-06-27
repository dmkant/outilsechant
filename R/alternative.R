#' alternative
#' @description Permet d'obtenir des quotas alternatif les plus proches des anciens
#'
#' @param data un jeu de donnée
#' @param constr un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs" et des na dans objectifs pour les quotas modifiables
#' @param taillech taille de l'echantillon
#' @param ancien anciens quotas
#'
#' @return un df comme constr avec les nouveaux quotas
#' @export
#' @import dplyr
#' @importFrom pracma eye
#' @importFrom pracma zeros
#' @importFrom lpSolve lp
#' @importFrom rlang .data
#' @examples
#' variables=c("Species","Species","Species")
#' modalites=c("setosa","versicolor","virginica")
#' objectifs=c(25,NA,NA)
#' constr=data.frame(variables=variables,modalites=modalites,
#' objectifs=objectifs,stringsAsFactors =FALSE)
#' taillech=110
#' ancien=c(25,60)
#' alternative(iris,constr,taillech,ancien)
alternative<-function(data,constr,taillech,ancien){
  f=faisable(data,constr)
  varint=as.character(unique(constr$variables[which(is.na(constr$objectifs))]))
  nbmod=constr%>%filter(is.na(.data$objectifs))%>%group_by(.data$variables)%>%summarise(n=length(.data$variables))
  nbmod=nbmod[match(varint,nbmod$variables),]
  if(1%in%nbmod$n){
    prob=as.character(nbmod$variables[which(nbmod$n==1)])
    ifelse(length(prob)==1,mes<-paste("Vous n'avez rentre qu'un NA pour la variable ",prob,collapse = " "),
           mes<-paste("Vous n'avez rentre qu'un NA pour les variables ",prob,collapse = " "))
    return(mes)
  }
  if(f$faisable==F){
    message("Infaisable")
    return("Ce n'est toujour pas faisable")
  }
  mat=matsyslin(data,constr)
  lignes=which(is.na(constr$objectifs))
  nbvide=length(lignes)
  nbpara=ncol(f$coefficient)
  #Contraintes supplementaire par variable qui a un na
  ##Indice par variable des modalite na
  indmod=constr%>%mutate(modalites=1:n())%>%filter(is.na(.data$objectifs))%>%group_by(.data$variables)%>%summarise(L=list(.data$modalites))
  indmod=indmod[match(varint,indmod$variables),]
  ##Variable qui a au moins un na
  suplcoef=lapply(indmod$L, function(x) colSums(mat[unlist(x),]%*%f$coefficient))
  suplconst=sapply(indmod$L, function(x) colSums(mat[unlist(x),]%*%f$constante))

  reste=constr%>%filter(.data$variables%in%varint)%>%group_by(.data$variables)%>%summarise(Som = sum(.data$objectifs,na.rm=T))
  reste=reste[match(varint,reste$variables),]
  suplega=(taillech-(reste$Som))-suplconst

  #Contrainte valeur absolue
  err1=mat[lignes,]%*%as.matrix(f$coefficient)
  diff1=ancien-mat[lignes,]%*%as.matrix(f$constante)

  nbvarsupl=nrow(err1)
  suplcoef2=rbind(cbind(-err1,eye(nbvarsupl)),cbind(err1,eye(nbvarsupl)))
  suplborn=c(-1*diff1,diff1)
  supldirection=rep(">=",2*nbvarsupl)

  err2=c(rep(0,ncol(err1)),rep(1,nbvarsupl))

  # Matrice de contraintes
  coeff=cbind(f$reduit$condition$coefficient,zeros(nrow(f$reduit$condition$coefficient),nbvarsupl))
  contraintes <- rbind(coeff,coeff,suplcoef2,cbind(fusion(suplcoef),zeros(nrow(fusion(suplcoef)),nbvarsupl)))

  # Bornes sup et inf des contraintes
  bornes <- c(f$reduit$condition$inf,f$reduit$condition$sup,suplborn,suplega)

  # Orientation des contraintes
  nbcont=nrow(f$reduit$condition$coefficient)
  constranints_direction  <- c(rep(">=",nbcont),rep("<=",nbcont),supldirection,rep("==",length(suplega)))

  optimum <-  lp(direction="min",
                 objective.in = err2,
                 const.mat = contraintes,
                 const.dir = constranints_direction,
                 const.rhs = bornes,
                 all.int = T)
  if (optimum$status!=0){return("Infaisable")}
  print(optimum$objval)
  utile=c("status","objval","solution")

  resu1=optimum$solution[1:nbpara]
  #Applique le resultat pour avoir les coeffient et le resultat
  X=as.matrix(f$coefficient)%*%as.matrix(resu1)+as.matrix(f$constante)
  resu=mat%*%X
  return(resu)
}
