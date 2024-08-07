#' Clutton-Brock et al 1979 index (CBI)
#'
#' @param mat matrix
#' @details The results of this function diverge from published examples in some
#' cases. While the function produces identical scores as the results in
#' \insertCite{gammell2003;textual}{EloRating} and
#' \insertCite{devries2000;textual}{EloRating} there are some slight deviations
#' for the example in \insertCite{whitehead2008;textual}{EloRating}. The final
#' example from \insertCite{bang2010;textual}{EloRating} is fairly off, but that
#' seems to be because these authors might have applied different definitions:
#' \insertCite{bang2010;textual}{EloRating} talk about 'who dominates' while
#' \insertCite{clutton-brock1979}{EloRating} consider 'who won interactions',
#' which are two very different conceptualizations, and which might explain the
#' discrepancies.
#' @return a named numeric vector with the indices for each individual
#' @export
#' @author Christof Neumann
#' @references
#' \insertRef{clutton-brock1979}{EloRating}
#'
#' \insertRef{bang2010}{EloRating}
#'
#' \insertRef{gammell2003}{EloRating}
#'
#' \insertRef{devries2000}{EloRating}
#'
#' \insertRef{whitehead2008}{EloRating}
#'
#'
#' @examples
#' # example from Gammell et al 2003 (table 1)
#' m <- matrix(0, nrow = 5, ncol = 5)
#' m[upper.tri(m)] <- 100
#' m[1, 5] <- 99
#' m[5, 1] <- 1
#' colnames(m) <- rownames(m) <- c("r", "s", "t", "u", "v")
#' m
#' CBI(m)
#'
#' # example from Whitehead 2008 (table 5.8, 5.9)
#' m <- c(0, 2, 0, 5, 2, 2, 1, 0, 2, 0,
#'        0, 0, 2, 2, 1, 0, 3, 2, 1, 1,
#'        0, 1, 0, 1, 1, 3, 1, 1, 4, 0,
#'        0, 0, 0, 0, 1, 1, 1, 0, 1, 0,
#'        0, 0, 0, 0, 0, 7, 1, 4, 2, 3,
#'        0, 0, 0, 0, 0, 0, 2, 3, 6, 10,
#'        0, 1, 1, 0, 2, 0, 0, 0, 0, 2,
#'        0, 0, 0, 1, 0, 0, 0, 0, 1, 1,
#'        0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
#'        0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#' mat <- matrix(m, nrow = 10, byrow = TRUE)
#' colnames(mat) <- rownames(mat) <- c("x907", "x915", "x912", "x910", "x917",
#'                                     "x898", "x897", "x911", "x904", "x902")
#' round(CBI(mat), 2)
#' # results in book:
#' # 33, 2.75, 3.08, 0.91, 0.86, 0.82, 0.92, 0.53, 0.23, 0.03
#'
#' simple_dom(mat2seq(mat)$winner, mat2seq(mat)$loser)
#'
#' # example from Bang et al 2010 (table 1)
#' m <- c(0, 1, 0, 2,
#'        1, 0, 4, 0,
#'        2, 2, 0, 3,
#'        3, 0, 1, 0)
#' m <- matrix(m, ncol = 4, byrow = TRUE)
#' m <- t(m)
#' colnames(m) <- rownames(m) <- letters[1:4]
#' CBI(m)
#' # results in paper:
#' # 1.43, 1, 0.7, 1
#'
#' # and from de Vries and Appleby (2000, table 4)
#' m <- c(0, 1, 1, 4, 0, 3, 6,
#'        0, 0, 1, 4, 0, 0, 0,
#'        0, 0, 0, 1, 1, 3, 14,
#'        0, 0, 0, 0, 2, 2, 1,
#'        0, 0, 0, 0, 0, 17, 2,
#'        0, 0, 0, 0, 0, 0, 12,
#'        0, 0, 0, 0, 0, 0, 0)
#' m <- matrix(m, ncol = 7, byrow = TRUE)
#' colnames(m) <- rownames(m) <- letters[1:7]
#' CBI(m)
#' simple_dom(mat2seq(m)$winner, mat2seq(m)$loser)


CBI <- function(mat) {
  # some checks
  if (nrow(mat) != ncol(mat)) {
    stop("matrix not square")
  }
  if (is.null(colnames(mat)) & is.null(rownames(mat))) {
    stop("either column names or row names are required")
  }
  # individual ids
  nms <- colnames(mat)
  if (is.null(nms)) {
    nms <- rownames(mat)
  }

  # results vectors
  b <- numeric(nrow(mat))
  db <- numeric(nrow(mat))
  l <- numeric(nrow(mat))
  dl <- numeric(nrow(mat))

  diag(mat) <- NA
  zmat <- mat > 0
  for (i in seq_len(nrow(mat))) {
    temp <- zmat
    temp[!temp] <- NA
    # i beat who
    b_names <- names(which(temp[i, ]))
    b[i] <- length(b_names)
    # exclude i
    temp <- temp[-i, -i]
    # strengths of opponents
    db[i] <- sum(rowSums(temp[b_names, , drop = FALSE], na.rm = TRUE))

    temp <- t(zmat)
    temp[!temp] <- NA
    # i lost to who
    l_names <- names(which(temp[i, ]))
    l[i] <- length(l_names)
    # exclude i
    temp <- temp[-i, -i]
    # weakness of opponents
    dl[i] <- sum(rowSums(temp[l_names, , drop = FALSE], na.rm = TRUE))
  }

  res <- (b + db + 1) / (l + dl + 1)
  names(res) <- colnames(mat)
  res
}
