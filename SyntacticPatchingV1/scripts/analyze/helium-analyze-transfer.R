#!/usr/bin/Rscript

## supress warning
options(warn=-1)


## load the library.
initial.options <- commandArgs(trailingOnly = FALSE)
## print(initial.options)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])

if (length(script.name) > 0) {
    ## it is probably loaded in emacs ESS
    script.basename <- dirname(script.name)
    other.name <- paste(sep="/", script.basename, "helium-analyze-lib.R")
    ## print(paste("Sourcing",other.name,"from",script.name))
    source(other.name)
} else {
    ## in that case, directly source the file
    print("In ESS??")
    source("helium-analyze-lib.R")
}


## source("helium-analyze-transfer-lib.R")

##############################
## The commands
##############################


## Getting the csv file from command line option
args = commandArgs(trailingOnly=TRUE)
## csvfile = "result.csv"
csvfile = args[1]
## csv = read.csv(csvfile, header=TRUE)



##############################
## Interface: helium-analyze-transfer.R /path/to/csv [fail/success/all]
## second argument is optional, default to fail
##############################

## data option: fail, all, success
if (length(args) == 1) {
    dataoption="fail"
} else {
    dataoption = args[2]
}

cat(paste("using data option: ", dataoption, "\n"))

## read the csv file
## do not replace [] with .
csv = read.csv(csvfile, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)



## filter the data
sub <- subset(csv, reach_code>=5)
## only the failure traces

if (dataoption == "fail") {
    sub <- subset(csv, reach_code==5 & status_code == 1)
} else if (dataoption == "success") {
    sub <- subset(csv, reach_code==5 & status_code == 0)
} else if (dataoption == "all") {
} else {
    cat(paste("Warning: data option ", dataoption, " is not valid.\n"))
    exit(1)
}
## sub <- subset(csv, reach_code==5)
## remove last two columns: (code)
sub <- sub[1:(length(csv)-2)]


## do the computation
ComputeTransferFunction(sub);

ComputeConstant(sub);

TransFromTwoInput(sub);

CheckArgvOneOf(sub)

## MultiTrans(sub);
