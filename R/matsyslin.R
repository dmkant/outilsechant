#' matsyslin
#'
#' @param data un jeu de donnée
#' @param constr un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs"
#'
#' @return la matrice du systeme linéaire ass
#' @export
#'
#' @examples
#' constr=data.frame(variables=c("Species","Species","Species"),
#' modalites=c("setosa","versicolor","virginica"),objectifs=c(98,123,100))
#' matsyslin(iris,constr)
matsyslin<-function(data,constr){
  #variable des quotas
  var=as.character(unique(constr[,1]))

  # Creation du tableau croise organiser de la meme mannière que le tableau de quota
  dispo=as.data.frame(table(data[,var]))
  if(length(var)>1){dispo=arrange_(.data = dispo,.dots = var)}

  ##Nombre de modalite par variable
  nbmod=constr%>%group_by(.data$variables)%>%summarise(n=length(.data$variables))
  nbmod=nbmod[match(var,nbmod$variables),] # Ordonner de la meme manière que le tableau de quota
  #Nombre d'equation
  neq=sum(nbmod$n)
  #Nombre d'inconnue
  ninc=prod(nbmod$n)
  A=matrix(0,nrow = neq,ncol = ninc)
  for(j in 1:length(var)){
    moda=unique(dispo[,j])
    ifelse(j==1,numli<-0,numli<-(numli+nbmod$n[j-1]) )
    for (i in 1:nbmod$n[j]){
      cond=which(dispo[,j]==moda[i])
      ifelse(j==1,A[i,cond]<-1,A[numli+i,cond]<-1)
    }
  }
  return(A)
}
