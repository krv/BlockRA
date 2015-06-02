#' Block ReArrangement Variance Equalizer
#'
#' @param matrix numeric matrix
#' @param epsilon target variance of row sums is epsilon multiplied by the mean of the matrix variances
#' @param shuffle randomly permute each column of the matrix before rearrangement
#' @return numeric matrix with a minimal row sum variance
#' @export
#' @seealso \code{\link{ra}} for the rearrangement algorithm
#' @seealso \code{\link{blockra}} for the block rearrangement algorithm
#' @examples
#' brave(matrix)
#' brave(matrix(1:5, 5, 3))
#' brave(matrix, epsilon = 0.001, shuffle = FALSE)
brave <- function(matrix, epsilon = 0.1, shuffle = TRUE) {
  if (shuffle) {
    matrix <- apply(matrix, 2, sample)
  }

  var.new   <- var(rowSums(matrix))
  var.old   <- 2 * var.new
  target    <- epsilon * mean(apply(matrix, 2, var))

  while ((var.new > target) && (var.new < var.old)) {
    partition <- equalvar(matrix)
    matrix    <- rearrangepartition(matrix, partition)
    var.old   <- var.new
    var.new   <- var(rowSums(matrix))
  }

  return(matrix)
}
