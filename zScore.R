generate_zscore <- function(input) {
  numColumns <- ncol(input)
  zscoreMatrix <- input[, 1, drop = FALSE]  # Preserve the gene IDs column
  
  for (i in 2:numColumns) {
    tpm <- as.numeric(input[, i])
    median_val <- median(log2(tpm + 1))
    sd_val <- sd(log2(tpm + 1))
    zscore <- (log2(tpm + 1) - median_val) / sd_val
    colname <- colnames(input)[i]
    zscoreMatrix <- cbind(zscoreMatrix, zscore)
    colnames(zscoreMatrix)[i] <- colname  # Preserve the column name
  }
  
  return(zscoreMatrix)
}
