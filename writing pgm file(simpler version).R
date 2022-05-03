write_pnm <- function(image, filename) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rearrange matrix and array values into the correct order
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dims        <- dim(image)
    values      <- t(image)
    dim(values) <- NULL
    nrow        <- dims[1]
    ncol        <- dims[2]
    magic       <- 'P5'  # single-channel image
    max_value=255L
    con <- file(filename, open = 'wb')
    on.exit(close(con))
    writeChar(paste0(magic, "\n", ncol, ' ', nrow, "\n", max_value, "\n"), con = con, eos = NULL)
    writeBin(values, con = con, size = 1L)
}

mat=matrix(sample(1:255,10000,replace=TRUE),ncol=100)
write_pnm(mat, "/Users/aytijhyasaha/Downloads/img3.pgm")
