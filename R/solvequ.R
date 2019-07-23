#' Solvequ
#' @description function qui facilite la résoltion de systeme lineaire
#' @param matsys matrice de coefficient du systeme lineaire
#' @param b vecteur d'egalies du systeme lineaire
#'
#' @return une liste qui indique si le systeme est compatible ainsi que les solutions
#' @export
#'
#' @examples
#' matsys=matrix(c(1,1,5,1,0,1),byrow=TRUE,ncol=3,nrow=2)
#' b=c(25,5)
#' solvequ(matsys,b)
solvequ<-function(matsys,b){
  #Pivot de gauss
  pvg=gaussianElimination(matsys,b)
  A2=pvg[,-ncol(pvg)]
  b2=pvg[,ncol(pvg)]

   #increase progress
  incProgress(0.15, detail = "Systeme lineaire : Verification existence de solutions")

  #Verification existence de solution
  ##Tous les dernierre lignes qui ne contiennes que des 0
  prescond=which(apply(A2,1,function(x) all(x==0)))
  nbcond=length(prescond)
  ## Condition d'existence de solution
  if (nbcond>0){
    #Presence de conditions
    existence=all(b2[prescond]==0) #verification des condition
    if ( existence==FALSE ){
      incProgress(0.15, detail = "Systeme lineaire : Mise en place des solutions")
      return(list(message("PAS OK"),raison="Les quotas son incompatibles",faisable=F,coefficient=A2,constantes=as.matrix(b2)))
      }
    A2=A2[-prescond,]
    if(!is.matrix(A2)){A2<-matrix(A2,nrow=1)}
    b2=b2[-prescond]
  }
  incProgress(0.15, detail = "Systeme lineaire : Mise en place des solutions")
  if (all(diag(A2)==1)){
    #Systeme echelonné avec des 1 sur la diagonale
    nbpara=dim(A2)[2]-dim(A2)[1]
    if (nbpara==0){return(list(faisable=T,coefficient=NULL,constantes=as.matrix(b2),varlibre=NULL))}
    diag(A2)=0
    Q=A2[,(ncol(A2)-nbpara+1):ncol(A2)]*(-1)
    varlibre=(ncol(A2)-nbpara+1):ncol(A2)
    #les variables libres sont les nbapara derniere variables dans ce cas
    Q=rbind(Q,eye(nbpara))
    constante=rbind(as.matrix(b2),zeros(nbpara,1))
    return(list(faisable=T,coefficient=Q,constantes=constante,varlibre=varlibre))
  }
  else {
    var=1:ncol(A2)
    #selectionne par ligne la première colone qui vaut 1
    if(nrow(A2)==2){
      A3=rbind(A2,rep(0,ncol(A2)))
      test=apply(A3,1,function(x) which(x==1))[-3]
      vardependante=unlist(lapply(test,min))
    }
    else{
      d=apply(A2,1,function(x) which(x==1))
      ifelse(is.matrix(d),vardependante<-apply(d,2,min),vardependante<-unlist(lapply(d,min)))
    }
    #les variables libres sont les autres
    varlibre=var[!(var%in%vardependante)]
    nbvarlibre=length(varlibre)
    #on prend toutes les varibles et on les multiplie par -1 pour les faire passer de l'autre côte de l'equation
    Q=A2[,varlibre]*(-1)
    if(!is.matrix(Q)){
      if(nbvarlibre==1){
        Q<-matrix(Q,ncol=1)
      }
      else{
        Q<-matrix(Q,nrow=1)
      }
    }
    # fonction qui creeé une fusionne une matrice identité de taille vec avec des matrice de 0 des deux autre coté pour completer
    creationsemiident<-function(vec){
      tailleindic=length(vec)
      zerogauche=zerosbis(tailleindic,length(which(varlibre<min(varlibre[vec]))))
      zerodroit=zerosbis(tailleindic,length(which(varlibre>max(varlibre[vec]))))
      return(cbind(zerogauche,eye(tailleindic),zerodroit))
    }
    #Mettre toutes les variables libres consecutives dans une meme liste
    if(nbvarlibre==1){
      indicensemble=list(1)
    }
    else{
      consecutif=c(T,sapply(2:nbvarlibre,function(x) varlibre[x]==varlibre[x-1]+1))
      indcoupure=c(1,which(!consecutif),nbvarlibre+1)
      indicensemble=lapply(1:(length(indcoupure)-1),function(x) seq(indcoupure[x],indcoupure[x+1]-1))
    }
    #creation des matrice semi identite
    tailleindic=sapply(indicensemble,length)
    coefvarlib=lapply(indicensemble,creationsemiident)
    constvarlibre=lapply(lapply(tailleindic,function(x) rep(0,x)),as.matrix)
    #savoir a quelle ligne on insere les matrice semi identite
    numligne=varlibre[sapply(indicensemble,min)]
    Q=insererlignes(as.data.frame(Q),coefvarlib,numligne)
    constante=insererlignes(as.data.frame(b2),constvarlibre,numligne)
    return(list(faisable=T,coefficient=as.matrix(Q),constantes=as.matrix(constante),varlibre=varlibre))
  }
}
