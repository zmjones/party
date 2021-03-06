
R2guide <- function(x, response, name) {

    dscname <- paste(name, ".dsc", sep = "")
    datname <- paste(name, ".dat", sep = "")

    cat(datname, "\n", file = dscname, append = FALSE)
    cat("NA\n", file = dscname, append = TRUE)
    cat("column, varname, vartype\n", file = dscname, append = TRUE)

    for (n in 1:ncol(x)) {

        cat(n, names(x)[n], " ", file = dscname, append = TRUE)
  
        if (names(x)[n] == response) {
            cat("d\n", file = dscname, append = TRUE)
        } else {
            cat(ifelse(is.factor(x[[n]]), "c", "n"), "\n",
                file = dscname, append = TRUE)
        }

    }

    write.table(x, sep = " ", file = datname, row.names = FALSE, col.names = FALSE)
}

complexity_quest <- function(file = "results.out") {
  nl <- readLines(file)
  nl <- nl[grep("Number of terminal nodes of final tree", nl)]
  nl <- as.numeric(strsplit(nl, " = ")[[1]][2]) 
  rval <- c(nl = nl, nc = nl)
  return(rval)  
}

foo <- function(x, response, bs, obvious = FALSE) {

    err <- rep(0, ncol(bs))
    npar <- rep(0, ncol(bs))
    for (i in 1:ncol(bs)) {

        learn <- x[rep(1:nrow(bs), bs[,i]),]
        test <- if(obvious) x[bs[,i] > 0,] else x[bs[,i] == 0,]

        R2guide(learn, response, "learn")
        R2guide(test, response, "test")

        a <- system("./questlinux < class.run", intern = TRUE)
        pred <- read.table("predict.txt", header = TRUE, 
                colClasses = rep("character", 4))[["predicted.label"]]
        pred <- factor(pred, levels = levels(test[[response]]))

        err[i] <- mean(test[[response]] != pred)
        npar[i] <- sum(complexity_quest()) - 1
        cat(i, " ", err[i], " ", npar[i], "\n")

        system("rm results.out")
        system("rm predict.txt")

    }
    rval <- list(error = err, npar = npar)
    return(rval)
}

