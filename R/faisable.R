#' Faisable
#' @description determine la fasabilite d'un tableau de quota par rapport à un jeu de donnees
#'
#' @param data un jeu de donnée
#' @param constr un tableau de quota de type df avec comme variable "variables", "modalitesé" et "objectifs"
#' @param taillech taille de l'échantillon
#'
#' @return une liste avec la faisabilité, les résultas des systemes linéaires associés
#' @export
#' @importFrom shiny  withProgress incProgress
#' @examples
#' #constr=data.frame(variables=c("Species","Species","Species"),
#' #modalites=c("setosa","versicolor","virginica"),objectifs=c(98,123,100))
#' #faisable(iris,constr)
faisable<-function(data,constr,taillech=NA){
  withProgress(message = "Determination de la faisabilite", style = getShinyOption("progress.style", default = "notification"), value = 0, {
    if(is.null(constr)){
      ifelse(taillech<=nrow(data),resu<-list(faisable=T),resu<-list(faisable=F,raison=paste("La taille de votre échantillon est supérieure au nombre de panélistes disponibles :", nrow(data))))
      return(resu)
    }
    var=as.character(unique(constr$variables)) #variable des quotas
    objectif=constr$objectifs
    ifelse(NA%in%objectif,enlever<-which(is.na(constr$objectifs)),enlever<--(1:length(objectif)))

    #Comparaison quota et disponibilite
    dispo1=lapply(var,function(x) as.data.frame(table(data[,x])))
    dispo1=fusion(dispo1)
    if(!all(dispo1$Freq[-enlever]>=objectif[-enlever])){
      incomp=which(dispo1$Freq[-enlever]<objectif[-enlever])
      ifelse(length(incomp)==1,mess<-paste0("La ligne ",incomp," presente un quota superieur aux disponibiltes du panel",collapse = " "),mess<-paste0("Les lignes ",paste0(incomp,collapse = ", ")," presentent des quotas superieurs aux disponibiltes du panel",collapse = " "))
      message("Impossible")
      constrmax<-constr
      constrmax$disponible<-dispo1$Freq
      constrmax$sup=dispo1$Freq[-enlever]<objectif[-enlever]
      resu=list(faisable=F,raison=mess,disponibilite=constrmax)
      return(resu)
    }

    # increase progress
    incProgress(0.1, detail = "Systeme lineaire : Pivot de Gauss")

    #resoudre systeme lineaire AX=B
    ##Mise en place du systeme lineaire
    A=matsyslin(data,constr)
    A2<-A[-enlever,]
    objectif2<-objectif[-enlever]
    if(!is.matrix(A2)){
      A2<-rbind(matrix(A2,nrow=1),0)
      objectif2<-c(objectif2,0)
    }
    ##Resolution
    if(is.logical(objectif2)){objectif2<-as.numeric(objectif2)}
    sol=solvequ(A2,objectif2)
    if(!sol$faisable){
      incProgress(0.9, detail = "Finit")
      resu=list(faisable=F,raison="La combinaison de vos quotas sont incompatible",systeme=sol)
      message("Impossible")
      return(resu)
    }

    # increase progress
    incProgress(0.15, detail = "Reduction systeme : Gestion des NA")

    #Condition sur les disponibilite
    dispo2=as.data.frame(table(data[,var]))
    if(length(var)>1){dispo2=arrange_(.data = dispo2,.dots = var)}
    ##Arguement solvineq
    if(is.null(sol$varlibre)){
      resu=list(matrice=A,coefficient=sol$coefficient,constantes=sol$constantes)
      if(!all(sol$constantes<=dispo2$Freq)){
        resu$faisable=F
        resu$raison="Effectif croise indisponible"
        message("IMPOSSIBLE")

        incProgress(0.45, detail = "Finit")
        return(resu)
      }
      else{
        resu$faisable=T
        resu$prespara=F
        message("PARFAIT")
        incProgress(0.45, detail = "Finit")
        return(resu)
      }
    }
    else{
      coefficient=rbind(sol$coefficient[-sol$varlibre,],sol$coefficient[sol$varlibre,])
      constante=matrix(c(sol$constantes[-sol$varlibre,],sol$constantes[sol$varlibre,]))
      disponible=c(dispo2$Freq[-sol$varlibre],dispo2$Freq[sol$varlibre])
      ncoeff=rbind(A%*%sol$coefficient,coefficient)
      nconst=rbind(A%*%sol$constantes,constante)
      nsup=matrix(c(dispo1$Freq,disponible),ncol=1)
      ninf=matrix(0,dim(ncoeff)[1],1)
      reduit=reduction(ncoeff,nconst,sup =nsup,inf=ninf)
      resu=list(matrice=A,coefficient=sol$coefficient,constantes=sol$constantes,reduit=reduit)
      if(all(reduit$statuts==0)){
        message("PARFAIT")
        resu$faisable=T
        resu$prespara=T
        incProgress(0.09, detail = "Finit")
        return(resu)
      }
      else{
        message("IMPOSSIBLE")
        resu$faisable=F
        resu$raison="Effectif croise indisponible"
        incProgress(0.09, detail = "Finit")
        return(resu)
      }
    }
  })

}
