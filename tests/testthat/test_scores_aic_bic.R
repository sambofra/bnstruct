library(bnstruct)

# Reference AIC/BIC for one family, computed directly in R:
#   logL = sum_{j,k} n_ijk log(n_ijk / n_ij)   (0 log 0 = 0)
#   AIC  = logL - q (r - 1)
#   BIC  = logL - q (r - 1) log(N) / 2
# The counts n_ijk use the rows with no missing value in the family;
# N = nrow(data) is the total number of rows.
reference_score <- function(data, ns, node, parents, type) {
  fam <- c(parents, node)
  N <- nrow(data)
  d <- data[stats::complete.cases(data[, fam, drop = FALSE]), , drop = FALSE]
  n <- nrow(d)
  r <- ns[node]
  q <- if (length(parents) > 0) prod(ns[parents]) else 1
  cfg <- if (length(parents) > 0) {
    interaction(as.data.frame(d[, parents, drop = FALSE]), drop = TRUE)
  } else {
    factor(rep(1L, n))
  }
  tab <- unclass(table(cfg, factor(d[, node], levels = seq_len(r))))
  n_ij <- rowSums(tab)
  loglik <- sum(ifelse(tab > 0, tab * log(tab / n_ij), 0))
  k <- q * (r - 1)
  if (type == "aic") loglik - k else loglik - k * log(N) / 2
}

bnstruct_score <- function(data, ns, node, parents, func) {
  .Call("bnstruct_score_node", data, ns, as.integer(node - 1L),
    as.integer(parents - 1L), as.numeric(func), 1.0,
    PACKAGE = "bnstruct"
  )
}

check_families <- function(dataset, families) {
  data <- as.matrix(raw.data(dataset))
  storage.mode(data) <- "integer"
  ns <- as.integer(node.sizes(dataset))
  for (fam in families) {
    node <- fam[1]
    parents <- fam[-1]
    expect_equal(bnstruct_score(data, ns, node, parents, 1),
      reference_score(data, ns, node, parents, "aic"),
      tolerance = 1e-8, info = paste("AIC, family", paste(fam, collapse = ","))
    )
    expect_equal(bnstruct_score(data, ns, node, parents, 2),
      reference_score(data, ns, node, parents, "bic"),
      tolerance = 1e-8, info = paste("BIC, family", paste(fam, collapse = ","))
    )
  }
}

test_that("AIC and BIC family scores match the textbook formulas (complete data)", {
  # families given as c(node, parents...)
  check_families(asia(), list(c(1), c(2, 1), c(6, 2, 4), c(8, 5, 6)))
})

test_that("AIC and BIC family scores match the textbook formulas (missing data)", {
  check_families(child(), list(c(1), c(2, 1), c(3, 2), c(5, 2, 3)))
})
