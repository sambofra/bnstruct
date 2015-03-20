#!/usr/bin/Rscript

library("bnstruct")

args     <- commandArgs(trailingOnly = TRUE)
myparams <- BNParams(args)

mydata <- child()
# mydata <- readRDS("/home/alberto/instances/1.rda")

net    <- BN(mydata)

net <- learn.structure(net, mydata, params=myparams)
# net <- learn.params(net, mydata, params=myparams)

plot(net)

 
