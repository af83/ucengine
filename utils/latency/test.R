#!/usr/bin/env Rscript

tbl <- read.csv("scores3.csv", sep=";", header=FALSE)
plot(tbl)
