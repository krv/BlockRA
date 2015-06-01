#' Rearrange matrix based on binary partition vector
#'
#' @param matrix numeric matrix
#' @param partition binary partition vector with 1 representing block 1
#' and 0 representing block 2
#' @return numeric matrix with a minimal row sum variance
#' @export
#' @examples
#' rearrangepartition(matrix, partition)
#' blockra(matrix(1:5, 5, 3), c(0, 1, 0))
rearrangepartition <- function(matrix, partition) {
  if (!is.matrix(matrix)) {
    stop("matrix argument is a numeric matrix")
  }

  if (!is.vector(partition)) {
    stop("partition is not a vector")
  }

  if (length(partition[!partition %in% c(0, 1)]) > 0) {
    stop("partition vector has elements that are not binary")
  }

  if (length(partition) != ncol(matrix)) {
    stop("Partition incompatible with matrix.")
  }

  # make sure block 1 is the smallest block
  if (sum(partition) < length(partition) / 2) {
    partition[partition = 1] <- 2
    partition[partition = 0] <- 1
    partition[partition = 2] <- 0
  }

  block1 <- matrix[, partition = 1]
  block2 <- matrix[, partition = 0]

  # rearrange block 1
  matrix[, partition == 1] <- rearrange(block1, block2)

  return(rearranged)
}
