contains <- function(s, pattern) {
    return (regexpr(pattern, s)[1] > -1);
}

startswith <- function(s, pattern) {
    return (regexpr(pattern,s)[1] == 1);
}

ComputeConstant <- function(data) {
    cat ("Constant output variable:\n");
    for (i in seq(length(data))) {
        col <- data[i];
        name <- names(data)[i];
        if (startswith(name, "output")) {
            ## omit na
            col <- na.omit(col);
            raw <- col[[1]]
            if (length(raw) < 3) next;
            value <- raw[1]
            if (length(which(raw != value)) == 0) {
                if (as.character(value)=="(nil)") {
                    cat(paste(name, "=",  "nil", "\n"))
                } else {
                    cat(paste(name, "=",  value, "\n"))
                }
            }
        }
    }
}

GetFormula <- function(yname, res, valid=1) {
    ## browser()
    ## print("Getting formula")
    b = res$coefficients[1]
    k = res$coefficients[-1]
    k = round(k, digit=3)
    b = round(b, digit=3)
    parts <- c()
    for (kname in names(k)) {
        if (is.na(k[kname])) next;
        if (k[kname] %% 1 != 0) next;
        if (k[kname]==1) {
            part <- paste(kname)
        } else if (k[kname]==0) {
            part <- ""
        } else {
            part <- paste(k[kname], "*", kname)
        }
        if (nchar(part) > 0) {
            parts <- c(parts, part)
        }
    }
    ## print(parts)
    if (length(parts) != valid) return ("");
    ## also add b
    if (b != 0) {
        parts <- c(parts, b);
    }
    rhs <- paste(parts, collapse=" + ")
    form <- paste(yname, "=", rhs)
    ## for unknown reason, the name string will add `` inside lm res.
    ## this statement remove them
    form <- gsub("`", "", form)
    return (form);
}


ComputeNumber <- function(frame, xname, yname) {
    names(frame)=c(yname,xname)
    res <- lm(frame)
    rsq <- summary(res)$r.squared
    if (!is.na(rsq) && rsq == 1) {
        form <- GetFormula(yname, res);
        cat(form, "\n")
    }
}

ComputeAddr <- function(frame, xname, yname) {
    ## cat("hello")
    ## print("frame1")
    ## print(frame[[1]])
    ## print("frame2")
    ## print(frame[[2]])

    ## print(frame[[1]] == frame[[2]])

    if (all(as.character(frame[[1]]) == as.character(frame[[2]]))) {
        cat(paste(yname,"=",xname, "\n"))
    }
}

ComputeTransferFunction <- function(data) {
    cat("Transfer functions:\n")
    for (i in seq(length(data))) {
        for (j in seq(length(data))) {
            if (i == j) next;
            xname <- names(data)[i]
            yname <- names(data)[j]
            ## only calculate transfer function for output versus input
            if (substr(xname, 1, 5) == "input" && substr(yname, 1, 6) == "output") {
                ## omit address
                subframe <- data[c(j,i)]
                subframe <- na.omit(subframe)
                if (dim(subframe)[1] < 3) next;
                if (contains(xname, "addr") && contains(yname, "addr")) {
                    ComputeAddr(subframe, xname, yname);
                } else if (contains(xname, "addr") || contains(yname, "addr")){
                    ## one of them is addr, do nothing
                } else {
                    ## both are numbers ?
                    ComputeNumber(subframe, xname, yname);
                }
            }
        }
    }
}

## The problem with this function is lm cannot treat NA as I want
## Thus now I have to support the fixed number of variables
## MultiTrans <- function(frame) {
##     OutputNameIndex=c()
##     InputNameIndex=c()
##     for (name in names(frame)) {
##         ## print(name)
##         if (contains(name, "addr")) next;
##         if (substr(name, 1, 6) == "output") {
##             OutputNameIndex=c(OutputNameIndex, name)
##         }
##         if (substr(name, 1, 5) == "input") {
##             InputNameIndex=c(InputNameIndex, name)
##         }
##     }
##     ## print(InputNameIndex)
##     ## print(OutputNameIndex)

##     for (name in OutputNameIndex) {
##         subframe <- frame[c(name, InputNameIndex)]
##         ## subframe <- na.omit(subframe)
##         res <- lm(subframe
##                   ## na.action = na.exclude
##                   ## na.action = na.omit
##                   ## na.action = na.fail
##                   ## na.action = NULL
##                   )
##         rsq <- summary(res)$r.squared
##         if (!is.na(rsq) && rsq == 1) {
##             ## print(summary(res))
##             form <- GetFormula(names(subframe)[1], res, valid=2);
##             ## cat(form,"\n")
##             print(form)
##         }
##     }
## }




TransFromTwoInput <- function(frame) {
    cat("Trans from two inputs:\n")
    OutputNameIndex=c()
    InputNameIndex=c()
    for (name in names(frame)) {
        ## print(name)
        if (contains(name, "addr")) next;
        if (substr(name, 1, 6) == "output") {
            OutputNameIndex=c(OutputNameIndex, name)
        }
        if (substr(name, 1, 5) == "input") {
            InputNameIndex=c(InputNameIndex, name)
        }
    }

    for (name in OutputNameIndex) {

        ## get combination of two variables
        for (i in seq(1, length(InputNameIndex))) {
            for (j in seq(i+1,length(InputNameIndex))) {
                if (i > length(InputNameIndex) ||
                    j > length(InputNameIndex)) {
                    break;
                }
                inputidx <- InputNameIndex[c(i,j)]
                subframe <- frame[c(name, inputidx)]
                subframe <- na.omit(subframe)
                if (dim(subframe)[1] < 3) next;
                res <- lm(subframe)
                rsq <- summary(res)$r.squared
                if (!is.na(rsq) && rsq == 1) {
                    k = res$coefficients[-1]
                    k = round(k, digit=3)
                    ## print(summary(res))
                    form <- GetFormula(names(subframe)[1], res, valid=2);
                    if (nchar(form)>0) {
                        cat(form,"\n")
                    }
                }
            }
        }
    }
}

## TransFromTwoInput(sub)


## use some test data doing testing
## Data Frame
## y=c(3,4,5)
## x1=c(1,2,4)
## x2=c(2,2,1)
## df = data.frame(y,x1,x2)
## res <- lm(df)
## summary(res)
## b=res$coefficients[1]
## k=res$coefficients[-1]


## Now I want one more operation: "one of"
## check if an output is "one of" a bench of input
## to check that, we do this:
## apply == on the vector of data frame
## collect all "true" column names
##
## this is not minimal
## so I want to, for now, only check the column of argv ...


CheckArgvOneOf <- function(data) {
    data=sub
    cat("Trans from two inputs:\n")
    OutputNameIndex <- c()
    ArgvNameIndex <- c()
    for (name in names(data)) {
        ## print(name)
        if (contains(name, "addr")) next;
        if (substr(name, 1, 6) == "output") {
            OutputNameIndex=c(OutputNameIndex, name)
        } else if (contains(name, "argv")) {
            ArgvNameIndex <- c(ArgvNameIndex, name)
        }
    }

    for (name in OutputNameIndex) {
        subframe <- data[c(name, ArgvNameIndex)];
        if (checkoneof(subframe)) {
            ## print out
            cat(paste(name,"=","oneofargv", "\n"))
        }
    }
}


## precondition: data[[1]] is the response
## data[2:] is all argvs
checkoneof <- function(data) {
    ## omit NA for response column
    subframe <- data[which(!is.na(data[1])),]
    if (length(dim(subframe))==0) return (FALSE);
    if (dim(subframe)[1] < 3) return (FALSE);
    ## extract the matrix
    res <- which(subframe[-1]==subframe[[1]], arr.ind=TRUE)
    ## the number of rows unique
    goodnum <- length(unique(res[,1]))
    ## compare the good number with the original rows
    if (dim(subframe)[1] == goodnum) {
        ## good
        ## however, the number might be slightly different. In this case, I would like to still consider as "one of"
    }

    total <- dim(subframe)[1]
    if ((total - goodnum)/total < 0.1) {
        ## print(total)
        ## print(goodnum)
        return (TRUE);
    }
    return (FALSE);
}
