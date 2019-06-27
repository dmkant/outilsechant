#' Optimisation de faisable
#' @description Cette fonction permet de determiner la faisabilité plus rapidement que la fonction faisable
#' @param varselect variable utilise pour diviser la tableau de quota
#' @param constr tableau de quota
#' @param taille taille de l'echantillon
#' @param data base de sondage
#'
#' @return une liste avec la faisabilite, la liste des bases de sondages, la liste des sous-tableau
#' @export
#' @importFrom lpSolve lp
#' @importFrom rlang .data
#' @import dplyr
#' @importFrom pracma eye
#' @examples
#' iris$couleur=sample(c("rose","blanc"),150,replace = TRUE)
#' constr=data.frame(variables=c("Species","Species","Species","couleur","couleur"),
#' modalites=c("setosa","versicolor","virginica","rose","blanc"),objectifs=c(25,25,25,35,40))
#' optimisation_f("couleur",iris,constr,75)
optimisation_f<-function(varselect,data,constr,taille){
  constr$objectifs2<-constr$objectifs/taille
  ind=which(constr$variables!=varselect)
  nbmodalite=which(constr$variables==varselect)
  constrbis=constr[ind,1:3]
  constrbis$objectifs<-NA
  listconstr=lapply(nbmodalite, function(x) constrbis)
  listancien=lapply(nbmodalite,function(x) floor(constr$objectifs[x]*constr$objectifs2[ind]))
  listdata=lapply(nbmodalite,function(x) data[which(data[,varselect]==constr$modalites[x]),] )
  taillech=constr$objectifs[nbmodalite]

  contraintes<-function(data,constr,taillech,ancien){
    f=faisable(data,constr)
    varint=as.character(unique(constr$variables[which(is.na(constr$objectifs))]))
    nbmod=constr%>%filter(is.na(.data$objectifs))%>%group_by(.data$variables)%>%summarise(n=length(.data$variables))
    nbmod=nbmod[match(varint,nbmod$variables),]

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
    ##Arbitrairement l'un des deux est négatif
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

    # Right hand side for the constraints
    bornes <- c(f$reduit$condition$inf,f$reduit$condition$sup,suplborn,suplega)

    # Orientation des contraintes
    nbcont=nrow(f$reduit$condition$coefficient)
    constranints_direction  <- c(rep(">=",nbcont),rep("<=",nbcont),supldirection,rep("==",length(suplega)))

    return(list(erreur=err2,contraintes=contraintes,bornes=bornes,direction=constranints_direction,nbpara=nbpara,coefficient=f$coefficient,constante=f$constantes,mat=cbind(mat%*%f$coefficient,zeros(nrow(mat),nbvarsupl))))

  }

  parametre=mapply(contraintes,data=listdata,constr=listconstr,taillech=taillech,ancien=listancien,SIMPLIFY = FALSE)

  fin=c(0,1:length(nbmodalite))*ncol(parametre[[1]]$contraintes)
  fin=fin[-1]
  debut=c(0,1:length(nbmodalite))*ncol(parametre[[1]]$contraintes)+1
  debut=debut[-length(debut)]
  debfin=cbind(debut,fin)

  elargissement<-function(matrice,ind,colone){
    result=matrix(0,ncol=colone,nrow=nrow(matrice))
    result[,ind[1]:ind[2]]<-matrice
    return(result)
  }
  l=list()
  l2=list()
  for(k in 1:length(nbmodalite)){
    l<-c(l,list(elargissement(parametre[[k]]$contraintes,debfin[k,],ncol(parametre[[1]]$contraintes)*length(nbmodalite))))
    l2<-c(l2,list(parametre[[k]]$mat))
  }
  contraintes=rbind(fusion(l),fusion2(l2))
  foncobj= unlist(lapply(parametre,function(x) x$erreur))
  bornes= c(unlist(lapply(parametre,function(x) x$bornes)),constr$objectifs[ind])
  direction=c(unlist(lapply(parametre,function(x) x$direction)),rep("==",length(constr$objectifs[ind])))


  resu=lp(direction="min",
          objective.in = foncobj,
          const.mat = contraintes,
          const.dir = direction,
          const.rhs = bornes,
          all.int = T)
  if(resu$status!=0){return(list(faisabl=FALSE,data=listdata,constr=listconstr))}
  for (k in 1:length(nbmodalite)){
    sol=resu$solution[debfin[k,1]:debfin[k,2]]
    X=as.matrix(parametre[[k]]$coefficient)%*%as.matrix(sol[1:parametre[[k]]$nbpara])+as.matrix(parametre[[k]]$constante)
    listconstr[[k]]$objectifs<-parametre[[k]]$mat[,1:parametre[[k]]$nbpara]%*%X
  }

  listfaisable=mapply(faisable,data=listdata,constr=listconstr,SIMPLIFY = FALSE)
  return(list(faisabl=listfaisable,data=listdata,constr=listconstr))
}


