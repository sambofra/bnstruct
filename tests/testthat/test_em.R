library(bnstruct)
context("Testing EM")

dataset <- asia()
network <- learn.network(dataset, algo="sm")
inf.engine <- InferenceEngine(network)

raw.data(dataset) <- rbind(raw.data(dataset), c(1,1,NA,NA,NA, NA, NA, NA))
em.out <- em(inf.engine, dataset)

updated.dataset <- em.out$BNDataset

test_that("length of imputed dataset", {
  expect_equal(nrow(imputed.data(updated.dataset)),
               nrow(raw.data(asia())) + 1)
})

test_that("all data imputed", {
  expect_equal(length(which(is.na(imputed.data(updated.dataset)))), 0)
})
