#!/usr/bin/env Rscript

par(mfrow=c(1,3))

tbl1 <- read.csv("scores1.csv", sep=";", header=FALSE)
tbl2 <- read.csv("scores2.csv", sep=";", header=FALSE)
tbl3 <- read.csv("scores3.csv", sep=";", header=FALSE)
lim = range(c(tbl1, tbl2, tbl3))
plot(tbl1, main="Single type", xlab="Batch size", ylab="latency µs", ylim=lim)
plot(tbl2, main="Two types", xlab="Batch size", ylab="latency µs", ylim=lim)
plot(tbl3, main="With a parent", xlab="Batch size", ylab="latency µs", ylim=lim)
