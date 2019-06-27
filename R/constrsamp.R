#' Construit un echantillon
#' @description  construit un echantillon selon la methode des quotas avec la methode pas-à-pas
#'
#' @param data un jeu de donnée
#' @param constr un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs"
#' @param foyer varaible qui sert d'identifiant foyer
#' @param nb taille de l'echantillon
#'
#' @return un echantillon
#' @export
#' @import shiny
#' @importFrom data.table data.table
#' @importFrom matrixStats rowProds
#' @importFrom stats rmultinom
#' @importFrom plyr .
#' @examples
#' #Pas d'exemple
constrsamp<-function(data,constr,foyer,nb){
  binarise=function(data,var){
    vecteur=data[,var]
    uni=unique(vecteur)
    out=matrix(0,ncol=length(uni),nrow=length(vecteur))
    colnames(out)=paste(var,"_",uni,sep="")
    for(i in 1:length(uni)){
      out[vecteur==uni[i],i]=1
    }
    out
  }

  # pb=txtProgressBar(style=3)
  constr["count"]=0
  constr["percent"]=0
  constr[,1]=as.character(constr[,1])
  id=data[,foyer]
  critere=unique(constr[,1])
  datasample=NULL
  for(i in 1:length(critere)){
    datasample=cbind(datasample,binarise(data,critere[i]))
  }
  datasample=t(datasample[,paste(constr[,1],"_",constr[,2],sep="")])
  keep=rep(1,ncol(datasample))
  pasel=rep(1,ncol(datasample))
  testa=datasample==0
  rali=which(!testa)%%nrow(testa)
  rali[rali==0]=nrow(testa)
  cali=ceiling(which(!testa)/nrow(testa))
  forced=F
  proba=rep(NA,ncol(datasample))
  frag=nb/100
  rows=rowSums(datasample)
  qsd=data.table(cali,rali)
  qsd=qsd[,.(lis=list(rali)),by=cali]
  qsd=qsd$lis
  qsd=do.call(rbind, qsd)
  withProgress(message="Tirage en cours:",value=0,{
    for(i in 1:nb){
      valty=as.numeric(constr[,3])-(as.numeric(constr[,4]))
      probs=((valty)/(as.numeric(constr[,3])))*abs(valty)/(rows+1)
      for(j in critere){
        tmp=probs[constr[,1]==j]
        mi=min(tmp)
        tmp=0.0000001+tmp-min(tmp)
        probs[constr[,1]==j]=tmp/sum(tmp)
      }

      qsd2=matrix(probs[qsd],ncol=ncol(qsd),nrow=nrow(qsd))
      proba=rowProds(qsd2)

      proba[pasel==0]=0
      nro=ncol(datasample)
      somme=sum(proba)
      proba=proba/somme
      tmp=which(rmultinom(1,size=1,proba)==1)
      if(length(tmp)>1){stop("tmp>1")}
      dtmp=datasample[,tmp]
      if(class(dtmp)=="matrix"){stop("mat")}
      supae=which(id==id[tmp])
      if(length(supae)>1){
        rows=rows-rowSums(datasample[,id==id[tmp]])
      }else{
        rows=rows-dtmp
      }
      constr[,4]=constr[,4]+dtmp
      keep[tmp]=0
      pasel[supae]=0
      if(i%%frag==0){incProgress(1/100,detail=paste(i*100/nb,"%"))}
    }
  })
  # close(pb)
  constr[,5]=round(as.numeric(constr$count)*100/as.numeric(constr[,3]),2)
  constr
  select=keep==0
  data=cbind(data,select)
  list(dataout=data,objectif=constr)
}
