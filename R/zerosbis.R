#' Zerosbis
#' @description crée une matrice de zéros
#'
#' @param n nombre
#' @param m nombre
#'
#' @return matrice de zéros
#' @export
#'
#' @examples
#' zerosbis(5.5,7.8)
zerosbis<-function (n, m = n) {
  stopifnot(is.numeric(n), length(n) <= 1, is.numeric(m), length(m) <= 1)
  n <- floor(n)
  m <- floor(m)
  if (n <= 0 || m <= 0 || is.null(n) || is.null(m) )
    return(NULL)
  else return(matrix(0, n, m))
}
