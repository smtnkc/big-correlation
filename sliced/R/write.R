#' Function: sliced.write
#' @title Creates external csv/txt files from very large virtual matrices
#' @description Cuts the large virtual matrices into slices and writes them to an external file slice by slice.
#'              It makes possible that creating very large files on limited system resources.
#' @author Samet Tenekeci
#' @param matrix A virtual matrix
#' @param file The output file
#' @param nblocks Number of blocks
#' @param verbose TRUE or FALSE
#' @return NULL
#' @export

sliced.write <- function(matrix, file, nblocks, verbose, ...) {
  ncols <- ncol(matrix)
  blocksize <- ncols %/% nblocks
  remainder <- blocksize + (ncols %% nblocks)

  if (verbose) {
    cat("ncols =", ncols, "\n")
    cat("blocksize =", blocksize, "\n")
    cat("remainder =", remainder, "\n")
    cat("nblocks =", nblocks, "\n")
    cat("matrix =", length(matrix), "\n\n")
  }

  for(i in 0:(nblocks-1)) {
    if (verbose) cat("Writing Block", i, "...\n")

    b <- (i*blocksize)+1
    if(i == (nblocks-1))
      e <- (i*blocksize)+remainder
    else
      e <- (i+1)*blocksize

    #if (verbose) {
    #  cat("b =", b, "\n")
    #  cat("e =", e, "\n")
    #}

    t <- round(as.data.frame(matrix[b:e,1:ncol(matrix)]),2)
    if(i==0) {
      write.table(t, file = file, append = FALSE, sep = ",", dec = ".", row.names = TRUE, col.names = NA)
    }
    else
    {
      write.table(t, file = file, append = TRUE, sep = ",", dec = ".", row.names = TRUE, col.names = FALSE)
    }
  }
  if (verbose) cat("DONE!\n")
}
