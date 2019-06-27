#' identique
#' @description  Renvoie une liste de meme taille que unique(a) avec les indices
#' @param a vecteur quelconque
#'
#' @return une liste avec les indice de chaque modalité
#' @export
#'
#' @examples
#' identique(c(1,2,3,1,3,2))
identique<-function(a){
  taille=length(a)
  vectaille=1:taille
  l=list()
  m=match(a,a)
  for (j in vectaille){
    if (!(j%in%unlist(l))){
      idtq=which(vectaille[j]==m)
      l=append(l,list(unique(c(j,idtq))))
    }
  }
  return(l)
}
