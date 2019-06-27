#' Insererligne
#'
#' @param data un jeu de donnees
#' @param NewRow un vecteur de m
#' @param RowNum numéro de la ligne
#'
#' @return data avec les lignes inserer au bon endroit
#' @export
#'
#' @examples #No exemple
insererlignes<-function(data, NewRow, RowNum = NULL){
  if((is.matrix(NewRow) | is.list(NewRow) & length(NewRow)==1) & length(RowNum)==1 ){
    if(is.list(NewRow)){NewRow<-NewRow[[1]]}
    if (ncol(data) != dim(NewRow)[2]) {
      stop("NewRow doit etre de la meme taille que le nombre de colone dans data.",
           call. = FALSE)
    }
    if (!is.null(RowNum) & RowNum!= (nrow(data)+1)) {
      LengthRow=dim(NewRow)[1]
      if(RowNum<= nrow(data)){
        data[seq(RowNum + LengthRow, nrow(data)+LengthRow), ] <- data[seq(RowNum, nrow(data)),]
        data[seq(RowNum,RowNum+LengthRow-1), ] <- NewRow
      }
      else(message("Ca sent la merde"))
    }
    else {
      colnames(NewRow)<-names(data)
      data <- data%>%rbind(NewRow)
    }
    return(data)
  }
  else{
    if (length(NewRow)!= length(RowNum)) {
      stop("NewRow must be the same length as the number Rownum.",
           call. = FALSE)
    }
    n=length(NewRow)
    return(insererlignes(insererlignes(data,NewRow[-n],RowNum[-n]),NewRow[n],RowNum[n]))
  }
}
