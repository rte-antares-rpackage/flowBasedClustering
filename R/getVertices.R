#' Get vertices from faces
#'
#' @param face \code{data.table}, face for 3 countries (BE, DE and FR)
#' @param b \code{numeric}, extreme point b
#'
#' @import pipeR
#'
#' @export
getVertices <- function(face, b){
  B <- as.matrix(face)
  IDfin <- 1:nrow(B)
  Un <- rep(1, 4)
  grid <- combn(1:nrow(B), 3)
  d <- b+1e-6
  res <- apply(grid, 2, function(gridRaw){
    # gridRaw <- grid[, X]
    Bijk<- rbind(B[gridRaw[1], ], B[gridRaw[1], ], B[gridRaw[2], ], B[gridRaw[3], ], Un)
    bijk <- c(b[gridRaw[1]], b[gridRaw[1]], b[gridRaw[2]], b[gridRaw[3]], 0)
    x <- try({
       qr.solve(Bijk, bijk)
    }, silent = TRUE)

    if("try-error" %in% class(x)){
      return(NULL)
    }

    if(all(B%*%x<=d)){
      return(x)
    }

  }) %>>%
    unlist %>>%
    matrix(ncol = 4, byrow = TRUE)
  res <- res[round(rowSums(res), 2) == 0,]
  DD <- dist(res, method = "euclidean", p = 2, upper = FALSE)
  DD <- as.matrix(DD)
  DD[lower.tri(DD, diag = TRUE)] <- 1
  res <- res[which(apply(DD, 2, min)>1e-6),1:3]
  res
}
