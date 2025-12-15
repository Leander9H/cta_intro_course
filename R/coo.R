require(tibble, quietly = TRUE)

top_pairs <- function(x, target, n = 15, return.data.frame = TRUE) {
  stopifnot(
    "`x` must be a dgCMatrix (e.g., a qunateda FCM object)" = inherits(x, "dgCMatrix"),
    "`target` word not in feature set." = target %in% rownames(x)
  )
  v <- as.numeric(x[target, ])
  names(v) <- rownames(x)
  v <- sort(v, decreasing = TRUE)
  v <- v[names(v) != target]
  v <- head(v, n)
  if (return.data.frame) {
    v <- cbind(
      tibble::tibble("target" = target),
      tibble::enframe(v, name = "feature", value = "count")
    )
  }
  return(v)
}

compute_pmi <- function(x, eps = 1e-12, positive = FALSE) {
  total <- sum(x)
  stopifnot(
    "`x` must be a qunateda FCM object" = inherits(x, "fcm"),
    "`x` has zero total counts." = (total <- sum(x)) > 0
  )
  
  # probabilities
  p_i  <- Matrix::rowSums(x)/total
  p_j  <- Matrix::colSums(x)/total
  
  # compute PMI: log( p_ij / (p_i %o% p_j) )
  nz <- Matrix::summary(x/total)   # i, j, x for nonzero entries
  
  # expected p(i)p(j) for those entries
  expected <- p_i[nz$i] * p_j[nz$j]
  vals <- log((nz$x + eps) / (expected + eps))
  
  if (positive)
    vals <- pmax(vals, 0)  # positive PMI
  
  # rebuild sparse matrix with PMI values at the same non-zero positions
  pmis <- Matrix::sparseMatrix(
    i = nz$i,
    j = nz$j,
    x = vals,
    dims = dim(x),
    dimnames = dimnames(x)
  )
  
  return(pmis)
}