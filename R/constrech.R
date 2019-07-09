#' Construit un echantillon
#' @description  construit un echantillon selon la methode des quotas avec la methode à deux niveaux
#' @param faisabl resultats de la fonction faisable()
#' @param data un jeu de donnée
#' @param constr un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs"
#' @param methode True pour l'approche probabiliste et False pour l'approche par optimisation
#'
#' @return un echantillon
#' @export
#' @importFrom rlang .data
#' @importFrom shiny  withProgress incProgress
#' @examples
#' #constr=data.frame(variables=c("Species","Species","Species"),
#' #modalites=c("setosa","versicolor","virginica"),objectifs=c(25,25,40))
#' #faisabl=faisable(iris,constr)
#' #iris$risque<-sample(seq(0,1,0.01),nrow(iris),TRUE)
#' #iris$tauxrep<-sample(seq(0,1,0.01),nrow(iris),TRUE)
#' #constrech(faisabl,iris,constr,TRUE)
#' #constrech(faisabl,iris,constr,FALSE)
constrech<-function(faisabl,data,constr,methode,nbsousech=NA){
  withProgress(message = "Construction de l'échantillon",value = 0, {
    var=unique(as.character(constr$variables))
    dispo=as.data.frame(table(data[,var]))
    if(length(var)>1){dispo=arrange_(.data = dispo,.dots = var)}
    else{
      names(dispo)<-c(var,"Freq")
    }
    ##Selection taille des effectifs ####
    if(faisabl$prespara==T){
      #Variable a maximiser
      ifelse(is.null(data$tauxrep),
             taux<-data%>%group_by_at(var)%>%summarize(Mean =1)%>%arrange_at(var),
             taux<-data%>%group_by_at(var)%>%summarize(Mean = mean(.data$tauxrep, na.rm=TRUE))%>%arrange_at(var)
             )
      tx=taux$Mean
      #Cas presence effectif disponible nul####
      nondispo=which(dispo$Freq==0)
      if(length(nondispo)>0){
        if(length(nondispo)==1){
          indicensemble=list(1)
        }
        else{
          consecutif=c(T,sapply(2:length(nondispo),function(x) nondispo[x]==nondispo[x-1]+1))
          indcoupure=c(1,which(!consecutif),length(nondispo)+1)
          indicensemble=lapply(1:(length(indcoupure)-1),function(x) seq(indcoupure[x],indcoupure[x+1]-1))

        }
        tailleindic=sapply(indicensemble,length)
        tauxna=lapply(tailleindic,function(x) matrix(0,nrow=x,ncol=1))
        #savoir a quelle ligne on insere taux$mea
        numligne=nondispo[sapply(indicensemble,min)]
        tx=insererlignes(as.data.frame(matrix(tx)),tauxna,numligne)
      }
      #Definition####
      coefficient=faisabl$coefficient
      m=matrix(mapply(multiplication,coefficient,as.matrix(tx)),ncol=ncol(coefficient))
      foncoptm=colSums(m)
      constante=colSums(faisabl$constante*as.matrix(tx))
      #################Parametre
      R=matrix(0,length(foncoptm))
      sysineq=faisabl$reduit
      ifelse(methode==T,R<-methodeprob(foncoptm,sysineq,R),R<-methodeopt(foncoptm,faisabl))
      if(methode==T & is.null(R)){
        R=matrix(0,length(foncoptm))
        R<-methodeopt(foncoptm,faisabl)
        chang=T
      }
      else{
        chang=F
      }
      X=faisabl$coefficient%*%R + faisabl$constantes
    }
    else{
      X=faisabl$constantes
      chang=F
    }
    if(!all(faisabl$matrice%*%X==constr$objectifs)){return("T'es dans la merde")}
    ##Selection des individus ####
    ind=which(X!=0)
    X=X[ind,]
    if(!all(X>=0)){print(X)}
    dispo=dispo[ind,]
    basesondage=merge(dispo%>%select(var),data, by = var)%>%arrange_at(var)
    
    minimum=c(1,sapply(1:length(dispo$Freq),function(x) sum(c(0,dispo$Freq[1:x]))+1)[-length(dispo$Freq)])
    maximum= sapply(1:length(dispo$Freq),function(x) sum(c(0,dispo$Freq[1:x])))
    nombre=cbind(minimum,maximum)
    if(!is.matrix(nombre)){nombre<-matrix(nombre,1)}
    seqcoup=1:nrow(nombre)
    ech<-function(t,basesondage1=NA){
      segment=nombre[t,1]:nombre[t,2]
      if(is.na(basesondage1)){basesondage1=basesondage}
      bsd=basesondage1[segment,]
      ifelse(is.null(bsd$risque),
             proba<-rep(1/nrow(bsd),nrow(bsd)),
             proba<-(1-bsd$risque)/sum(1-bsd$risque)
             )
      if(length(which(proba==0))>0){proba[which(proba==0)]<-0.00000001}
      if(length(segment)>1){
        echant=sample(segment,X[t],prob = proba,replace = F)
      }
      else{
        echant=segment
      }
      return(echant)
    }
    dupech<-function(x){
      bsd=basesondage
      echantillon=list()
      supprimer=c()
      for (i in 1:x){
        a=unlist(sapply(seqcoup,ech))
        echantillon=c(echantillon,list(bsd[a,]))
        supprimer=c(supprimer,a)
        bsd=basesondage[-supprimer,]
      }
      return(echantillon)
    }
    if(is.na(nbsousech)){
      echantillon=basesondage[unlist(sapply(seqcoup,ech)),]
    }
    else{
      echantillon=dupech(nbsousech)
    }
    #FIN ####
    return(list(echantillon=echantillon,changement=chang))
  })
}
