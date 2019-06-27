#' gaussianElimination
#' pivot de gauss
#' @param A matrice
#' @param B matrice
#' @param tol inutile
#' @param verbose inutile
#' @param latex inutile
#' @param fractions inutile
#'
#' @return matrice
#' @export
#'
#' @examples
#' #no
gaussianElimination<-function (A, B, tol = sqrt(.Machine$double.eps), verbose = FALSE,
          latex = FALSE, fractions = FALSE)
{
    if ((!is.matrix(A)) || (!is.numeric(A)))
        stop("argument must be a numeric matrix")
    n <- nrow(A)
    m <- ncol(A)
    det <- 1
    pivots <- rep(0, n)
    interchanges <- 0
    reduced <- TRUE
    if (!is.null(attr(A, "reduced"))) {
        reduced <- attr(A, "reduced")
        attr(A, "reduced") <- NULL
    }
    if (!missing(B)) {
        B <- as.matrix(B)
        if (!(nrow(B) == nrow(A)) || !is.numeric(B))
            stop("argument must be numeric and must match the number of row of A")
        A <- cbind(A, B)
    }
    i <- j <- 1
    if (verbose) {
        cat("\nInitial matrix:\n")
        printMatrix(A)
    }
    while (i <= n && j <= m) {
        if (verbose)
            cat("\nrow:", i, "\n")
        while (j <= m) {
            currentColumn <- A[, j]
            currentColumn[1:n < i] <- 0
            which <- which.max(abs(currentColumn))
            pivot <- currentColumn[which]
            det <- det * pivot
            pivots[i] <- pivot
            if (abs(pivot) <= tol) {
                j <- j + 1
                next
            }
            if (which > i) {
                A <- rowswap(A, i, which)
                det <- -det
                interchanges <- interchanges + 1
                if (verbose) {
                    cat("\n exchange rows", i, "and", which, "\n")
                    printMatrix(A)
                }
            }
            A <- rowmult(A, i, 1/pivot)
            if (verbose && abs(pivot - 1) > tol) {
                cat("\n multiply row", i, "by", if (fractions)
                    as.character(MASS::fractions(1/pivot))
                    else 1/pivot, "\n")
                printMatrix(A)
            }
            for (k in 1:n) {
                if (k == i)
                    next
                if (!reduced && k < j)
                    next
                factor <- A[k, j]
                if (abs(factor) < tol)
                    next
                A <- rowadd(A, i, k, -factor)
                if (verbose) {
                    if (abs(factor - 1) > tol) {
                        cat("\n multiply row", i, "by", if (fractions)
                            as.character(MASS::fractions(abs(factor)))
                            else abs(factor), if (factor > 0)
                                "and subtract from row"
                            else "and add to row", k, "\n")
                    }
                    else {
                        if (factor > 0)
                            cat("\n subtract row", i, "from row", k,
                                "\n")
                        else cat("\n add row", i, "from row", k,
                                 "\n")
                    }
                    printMatrix(A)
                }
            }
            j <- j + 1
            break
        }
        i <- i + 1
    }
    zeros <- which(apply(A[, 1:m], 1, function(x) max(abs(x)) <=
                             tol))
    if (length(zeros) > 0) {
        zeroRows <- A[zeros, ]
        A <- A[-zeros, ]
        A <- rbind(A, zeroRows)
    }
    rownames(A) <- NULL
    A
}
