generate_rpkm <- function(input) {
  numColumns <- ncol(input)
  rpkmMatrix <- input[, 1]  # Preserve the gene IDs column
  
  # Calculate RPKM values for each column
  for (i in 2:(numColumns - 1)) {
    data <- input[, i]
    lengths <- input$gene_length
    totalReads <- sum(as.numeric(input[, i]))  # Total mapped reads per sample
    rpkm <- (10 ^ 9) * (as.numeric(data)) / (as.numeric(lengths) * totalReads)
    colname <- colnames(input)[i]
    rpkmMatrix <- cbind(rpkmMatrix, rpkm)
    colnames(rpkmMatrix)[i] <- colname  # Preserve the column name
  }
  
  return(rpkmMatrix)
}