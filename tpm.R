# Function to calculate TPM values
generate_tpm <- function(input) {
  numColumns <- ncol(input)
  tpmMatrix <- input[, 1]  # Preserve the gene IDs column
  
  for (i in 2:(numColumns - 1)) {
    data <- input[, i]
    lengths <- input$gene_length
    # totalReads <- sum(as.numeric(input[, i]))
    tpm <-
      (as.numeric(data) / as.numeric(lengths)) / sum(as.numeric(data) / as.numeric(lengths)) * 1e6
    colname <- colnames(input)[i]
    tpmMatrix <- cbind(tpmMatrix, tpm)
    colnames(tpmMatrix)[i] <- colname  # Preserve the column name
  }
  
  return(tpmMatrix)
}