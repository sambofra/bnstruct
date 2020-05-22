library(bnstruct)
context("Testing EM with continuous data")

skip_on_cran()

contdatafile <- system.file("extdata", "contdata.txt", package="bnstruct")
m <- read.table(contdatafile, header=FALSE, sep="\t")

dataset <- BNDataset(data = m,
                     discreteness = rep('c',4),
                     variables = c("X1" , "X2" , "X3" , "X4"),
                     node.sizes = rep(4,4),
                     num.time.steps=3)

dbn <- learn.dynamic.network(dataset, num.time.steps=3)

inf.engine <- InferenceEngine(dbn)
new_obs <- c(3.4, 1-6, -0.2, 1 ,rep(NA,8))
raw.data(dataset) <- rbind(raw.data(dataset), new_obs)

out.em <- em(inf.engine, dataset)
updated <- imputed.data(out.em$BNDataset)

test_that("all data imputed", {
  expect_equal(length(which(is.na(updated))), 0)
})

