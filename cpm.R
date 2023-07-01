generate_cpm <- function(input) {
  numColumns <- ncol(input)
  cpmMatrix <- input[, 1]  # Preserve the gene IDs column
  
  for (i in 2:(numColumns - 1)) {
    data <- input[, i]
    totalReads <- sum(data)
    cpm <- (data / totalReads) * 1e6
    colname <- colnames(input)[i]
    cpmMatrix <- cbind(cpmMatrix, cpm)
    colnames(cpmMatrix)[i] <- colname  # Preserve the column name
  }
  
  return(cpmMatrix)
}