#!/usr/bin/Rscript
## supress warning
options(warn=-1)
args = commandArgs(trailingOnly=TRUE)
csvfile = args[1]
csv = read.csv(csvfile, header=TRUE)
total_test <- dim(csv)[[1]]
sub = subset(csv, reach_code>=5)
total_reach_poi <- dim(sub)[[1]]
sub = subset(csv, reach_code==5 & status_code == 1)
total_fail_poi <- dim(sub)[[1]]

cat("Total test: ", total_test, "\n")
cat("Total reach poi: ", total_reach_poi, "\n")
cat("Total fail poi: ", total_fail_poi, "\n")
