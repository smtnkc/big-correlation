#' Function: sliced.write
#' @title Constructs very large correlation matrices
#' @description Cuts the large data frames into slices, calculates correlations, and combines them back.
#'              It makes possible that constructing very large correlation matrices on limited system resources.
#' @author Samet Tenekeci
#' @param dataframe A data frame object
#' @param nblocks Number of blocks
#' @param cmethod Correlation method
#' @param verbose TRUE or FALSE
#' @return A virtual matrix
#' @import ff
#' @export

sliced.cor <- function(dataframe, nblocks, cmethod, verbose, ...) {
  ncols <- ncol(dataframe)
  blocksize <- ncols %/% nblocks
  remainder <- blocksize + (ncols %% nblocks)

  corMatrix <- ff(vmode = "single", dim = c(ncols, ncols))

  if (verbose) {
    cat("ncols =", ncols, "\n")
    cat("blocksize =", blocksize, "\n")
    cat("remainder =", remainder, "\n")
    cat("nblocks =", nblocks, "\n")
    cat("corMatrix =", length(corMatrix), "\n\n")
  }

  for (i in 0:(nblocks-1)) {
    for(j in 0:(nblocks-1)) {

      iRb <- (i*blocksize)+1
      if(i == (nblocks-1))
        iRe <- (i*blocksize)+remainder
      else
        iRe <- (i+1)*blocksize

      jRb <- (j*blocksize)+1
      if(j == (nblocks-1))
        jRe <- (j*blocksize)+remainder
      else
        jRe <- (j+1)*blocksize

      iRange <- iRb:iRe
      jRange <- jRb:jRe

      # if (verbose) cat("iRange:", iRange, "jRange", jRange, "\n")

      tempMatrix <- cor(dataframe[, iRange], dataframe[, jRange], method = cmethod)
      corMatrix[iRange, jRange] <- round(tempMatrix,2)
      if (verbose) cat("Correlation between block", i, "and block", j, "is calculated.\n")
      tempMatrix <- NULL
    }
  }
  if (verbose) cat("DONE!\n")
  gc() # garbage collection
  return(corMatrix)
}
