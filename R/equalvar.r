#' Find two blocks with equal variance
#'
#' @param matrix numeric matrix
#' @return numeric binary vector with the partition
#' @export
#' @examples
#' equalvar(matrix)
#' equalvar(matrix(1:5, 5, 3))
equalvar <- function(matrix) {
  swept       <- matrix - tcrossprod(rep(1, nrow(matrix)), colMeans(matrix))
  covariances <- crossprod(swept, rowSums(swept)) / nrow(swept - 1)

  block1 <- 0
  block2 <- 0

  covariances <- sort(covariances, index.return = TRUE)
  partition   <- rep(0, length(covariances$x))

  for (element in 1 : length(covariances$x)) {
    value <- covariances$x[element]

    if (value > 0) {
      if (block1 < block2) {
        block1 <- block1 + value
        partition[covariances$ix[element]] <- 1
      } else {
        block2 <- block2 + value
      }
    } else {
      if (block1 > block2) {
        block1 <- block1 + value
        partition[covariances$ix[element]] <- 1
      } else {
        block2 <- block2 + value
      }
    }
  }

  return(partition)
}
