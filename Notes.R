WaldTest <- function(fit = NULL, digits = 3, cons = c("==0"), order = TRUE,
                     only_trim = FALSE, full = FALSE, keep_label = FALSE,
                     remove_def = TRUE, all = TRUE) {
    ## Piece of code slightly modified from the lavTestWald function in lavaan.
    ## This chunk made a data.frame from the constraints argument.
    CON <- attr(lavaan::lavParseModelString(cons), "constraints")
    LIST <- data.frame(unlist(lapply(CON, "[[", "lhs")),
                       unlist(lapply(CON, "[[", "op")),
                       unlist(lapply(CON, "[[", "rhs")))
    colnames(LIST) <- c("lhs", "op", "rhs")
    ## LIST[LIST == ""] <- NA;; sum(LIST == "")
    ## !complete.cases(LIST)
    ## LABELS
    ## nlabel <- fit@ParTable$label[fit@ParTable$label != ""]
    back <- fit@ParTable
    if (all == TRUE) {
        par_table <- as.data.frame(fit@ParTable, stringsAsFactors = FALSE)
        free_nr <- (par_table$lhs == par_table$rhs) +
            (par_table$free < 1) +
            !(par_table$label == "")
        par_table$label[free_nr == 0] <- paste0(
            "l", 1:length(par_table$label[free_nr == 0]))
        fit@ParTable$label <- par_table$label
    }
    par_est <- lavaan::parameterEstimates(fit, standardized = TRUE,
                                          remove.def = remove_def)
    par_est <- par_est[!par_est$label == "", ]
    characters <- par_est[, 1:4]
    numbers <- par_est[, 5:ncol(par_est)]
    if (nrow(LIST) == 1 & sum(LIST == "") == 1) {
        con <- paste0(par_est$label, cons)
    } else if (nrow(LIST) > 1 & sum(LIST == "") == 1) {
        cons <- unlist(strsplit(cons, "\n"))
        w_true <- which(LIST == "", TRUE)
        a_label <- par_est$label %in% c(as.character(LIST$lhs),
                                        as.character(LIST$rhs))
        ## con <- c(paste(cons[cons != cons[w_true[1]]], collapse = "\n"), paste0(par_est$label[!a_label], cons[w_true[1]]))
        ## t(sapply(d, function(x) grepl(x, cons)))
        ## sum(grepl(",", cons)) == 1
        con <- par_est$label
        con[a_label] <- cons[cons != cons[w_true[1]]]
        con[!a_label] <- paste0(par_est$label[!a_label], cons[w_true[1]])
        ## con <- c(cons[cons != cons[w_true[1]]],
        ##          paste0(par_est$label[!a_label], cons[w_true[1]]))
    } else if (nrow(LIST) >= 1 & sum(LIST == "") == 0 & all == TRUE) {
        cons <- unlist(strsplit(cons, "\n"))
        a_label <- par_est$label %in% c(as.character(LIST$lhs),
                                        as.character(LIST$rhs))
        con <- par_est$label
        con[a_label] <- cons
        con[!a_label] <- paste0(par_est$label[!a_label], "==0")
    } else if (nrow(LIST) >= 1 & sum(LIST == "") == 0 & all == FALSE) {
        con <-  unlist(strsplit(cons, "\n"))
        ## con <- cons
    }
    wald <-  sapply(con, lavaan::lavTestWald, object = fit)
    dframe <- data.frame(matrix(unlist(wald), nrow = ncol(wald), byrow = T),
                         stringsAsFactors = FALSE)

    const    <- colnames(wald)
    stat     <- round(as.numeric(dframe$X1), digits)
    df       <- as.numeric(dframe$X2)
    wald.pv  <- round(as.numeric(dframe$X3), digits)
    type.se  <- dframe$X4

    out <- data.frame(characters, const, stat, df, wald.pv, type.se, numbers)
    out[, -c(1:5, 9)] <- round(out[, -c(1:5, 9)], digits)

    if (keep_label == FALSE) {
        fit@ParTable <- back
    }

    if (full == FALSE) {
        out <- out[, 1:9]
    }

    if (order == TRUE) {
        out <- out[order(out$stat), ]
    }

    if (only_trim == TRUE) {
        out <- subset(out, wald.pv >= .05)
    }
    out
}


###########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

WaldTest <- function(fit = NULL, digits = 3, cons = c("==0"), order = TRUE, only.trim = FALSE, full = FALSE, keep.label = FALSE, remove.def = TRUE, all = FALSE)
{
    h <- fit@ParTable$label
    h <- h[h !=""]
    if (length(h) == 0 | all == TRUE) { ## Remains pending: DO NOT erase users labels when all=TRUE
        back <- fit@ParTable
        PT <- as.data.frame(fit@ParTable, stringsAsFactors = FALSE)
        g <- (PT$lhs == PT$rhs) + (PT$free < 1)
        x <- PT[g == FALSE,] ## 0 is always FALSE for R, 1 or more is TRUE
        PT$label[g == 0] <- paste0("l",1:length(PT$label[g == 0]))
        fit@ParTable$label <- PT$label
        
        ## fit@ParTable$label <- c(paste0("label",1:length(x$label)), strrep("", 1:(length(fit@ParTable$label)-length(x$label))))

        ## PT$label[g == 0] <- paste0("l",1:length(PT$label[g == 0]))

        c <- parameterEstimates(fit, standardized = TRUE, remove.def = remove.def)
        c <-  c[!c$label== "",]
        f <- c[,1:4]
        e <- c[,5:ncol(c)]
        
    } else {
        c <- parameterEstimates(fit, standardized = TRUE, remove.def = remove.def)
        ##  g<- c$lhs == c$rhs;  x <- c[g == FALSE,]; x$label <- paste0("l",1:nrow(x))
        ## Es necesario cambiar la misma tabla del fit, para eso ver "lavTestWald"
        ## Para agregar espacios en blanco a un vector c(a,strrep("", 1:5))
        c <-  c[!c$label== "",]
        f <- c[,1:4]
        e <- c[,5:ncol(c)]
    }
    
    ## a <- lavInspect(fit, "list")$label
    ## a <- a[a !=""]
    if (identical(cons,"==0")) {
        a2 <- c$label
        con <- paste0(a2, cons)
    } else {
        con <- cons
    }
    
    a <-  (sapply(con, lavTestWald, object = fit))
    d <- data.frame(matrix(unlist(a), nrow=ncol(a), byrow=T),
                    stringsAsFactors=FALSE)

    const    <- colnames(a)
    stat     <- round(as.numeric(d$X1), digits)
    df       <- as.numeric(d$X2)
    wald.pv  <- round(as.numeric(d$X3), digits)
    type.se  <- d$X4

    b <- data.frame(f, const, stat, df, wald.pv, type.se, e)
    b[,-c(1:5,9)] <- round(b[,-c(1:5,9)], digits)

    if(keep.label == FALSE) { # Remains Pending, if new labels are really on the ParTable.
        fit@ParTable <- back
    }

    if(full == FALSE) {
        b <- b[,1:9]
    }

    if(order == TRUE) {
        b <- b[order(b$stat),]
    }

    if(only.trim == TRUE) {
        b <- subset(b, wald.pv >= .05)
    }
    b
}

mardiaTest <- function(data, digits = 3) {

    mardia <- function(data, digits) {
        Skew <- mardiaSkew(data)
        Kurtosis <- mardiaKurtosis(data)
        Kurtosis[4] <- Kurtosis[3]
        Kurtosis[3] <- ""
        Kurtosis <- as.numeric(Kurtosis)
        a <- as.data.frame(rbind(Skew, Kurtosis))
        colnames(a) <- c(abbreviate("multivariate", 5), "Statistic", "df", "p.value")
        a <- round(a, digits)
        a$Result <-  ifelse(a$p.value > 0.05, "Yes", "No")
        a
    }
    if (inherits(data, "list") == TRUE) {
        lapply(dat, function(x) mardia(x, digits))
    } else {
        mardia(data, digits)
    }
}

mardia <- function(data, digits = 3) {
    
    Skew <- mardiaSkew(data)
    Kurtosis <- mardiaKurtosis(data)
    Kurtosis[4] <- Kurtosis[3]
    Kurtosis[3] <- ""
    Kurtosis <- as.numeric(Kurtosis)
    a <- as.data.frame(rbind(Skew, Kurtosis))
    colnames(a) <- c(abbreviate("multivariate", 5), "Statistic", "df", "p.value")
    a <- round(a, digits)
    a$Result <-  ifelse(a$p.value > 0.05, "Yes", "No")
    a
}

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

## t(lavTestWald(fit, constraints = paste0(lavInspect(fit,"list")$label,c("==0 "), collapse  ="\n ")))

## a <- lavInspect(fit, "list")$label
## a <- a[a !=""]
## con <- paste0(a, c("== 0"))
## p.value <- format(round(as.numeric(d$X3), digits), nsmall = digits)
## con <- paste0(a, c("== 0"))

WaldTest <- function(fit = NULL, digits = 3, cons = c("==0"), order = TRUE, only.trim = FALSE, full = FALSE)
{
    c <- parameterEstimates(fit, standardized = TRUE, remove.def = TRUE)
    ##  g<- c$lhs == c$rhs;  x <- c[g == FALSE,]; x$label <- paste0("l",1:nrow(x))
    ## Es necesario cambiar la misma tabla del fit, para eso ver "lavTestWald"
    ## Para agregar espacios en blanco a un vector c(a,strrep("", 1:5))
    c <-  c[!c$label== "",]
    f <- c[,1:4]
    e <- c[,5:ncol(c)]
    
    ## a <- lavInspect(fit, "list")$label
    ## a <- a[a !=""]
    a <- c$label
    con <- paste0(a, cons)
    
    a <-  (sapply(con, lavTestWald, object = fit))
    d <- data.frame(matrix(unlist(a), nrow=ncol(a), byrow=T),
                    stringsAsFactors=FALSE)

    const    <- colnames(a)
    stat     <- round(as.numeric(d$X1), digits)
    df       <- as.numeric(d$X2)
    wald.pv  <- round(as.numeric(d$X3), digits)
    type.se  <- d$X4

    b <- data.frame(f, const, stat, df, wald.pv, type.se, e)
    b[,-c(1:5,9)] <- round(b[,-c(1:5,9)], digits)

    if(full == FALSE) {
        b <- b[,1:9]
    }

    if(order == TRUE) {
        b <- b[order(b$stat),]
    }

    if(only.trim == TRUE) {
        b <- subset(b, wald.pv >= .05)
    }
    b
}

## a[order(a$stat),];  subset(a, p.value >=.05)


## don't know yet how to coerce in a simple way this complex matrix to a data frame, so I use the long long way
## a <-  t(sapply(con, lavTestWald, object = fit))

## stat <- do.call(rbind, lapply(a[,1], data.frame))
## cons <- rownames(stat)
## stat <- stat[,1]
## df   <- do.call(rbind, lapply(a[,2], data.frame))
## df   <- df[,1]
## p.value   <- do.call(rbind, lapply(a[,3], data.frame))
## p.value   <-  format(round(p.value[,1], 4), nsmall = 4)
## se   <- do.call(rbind, lapply(a[,4], data.frame))
## se   <- se[,1]

## b <- data.frame(stat = stat, df = df, p.value = p.value, se = se)
## row.names(b) <- cons
## print(b)

## https://stackoverflow.com/questions/4227223/r-list-to-data-frame
## https://stackoverflow.com/questions/33240333/convert-character-to-number-without-the-loss-of-decimal-in-r
## https://stackoverflow.com/questions/44058282/complex-list-to-data-frame?noredirect=1&lq=1

## data.frame(matrix(unlist(a), nrow=nrow(a), byrow=T),stringsAsFactors=FALSE) - another way, IT NEEDS THE DATA WITHOUT TRANSPOSING.    
## library(plyr)
## stat <- ldply(a[,1], data.frame)
## cons <- stat$.id
## stat <- stat[,-c(1)]
## df   <- ldply(a[,2], data.frame)
## df   <- df[,-c(1)]
## p.value <- ldply(a[,3], data.frame)
## p.value <- p.value[,-c(1)]
## se <- ldply(a[,4], data.frame)
## se <- se[,-c(1)]

## b <- data.frame(stat = stat, df = df, p.value = p.value, se = se)
## row.names(b) <- cons
## print(b)

parEst <- function(fit = NULL, std = TRUE, simple = TRUE)
{ require(lavaan)
    parme <- parameterEstimates(fit, standardized = std)

    if (simple == TRUE) {
        param <- subset(parme, op == "=~")
        h2 <- param$std.all^2
        u2 <- 1 - h2
        x <- data.frame(h2, u2) ## h2 - Communality; u2 - Uniqueness
        p <- merge(data.frame(parme, row.names=NULL),
                   data.frame(x, row.names=NULL),
                   by = 0, all = TRUE, sort = F)
        p$Row.names <- as.numeric(p$Row.names)
        p <-  p[order(p$Row.names),][-1]
        p[,-c(1:3)] <- round(p[,-c(1:3)],4)
        p   
    } else {
        param <- parme
        h2 <- param$std.all^2
        u2 <- 1 - h2
        p <- cbind(param, h2, u2)
        p[,-c(1:3)] <- round(p[,-c(1:3)],4)
        p
    }
}

pretty.fit <- function(fit) {
    lavaan:::print.fit.measures(fitMeasures(fit)) 
}

core.fit <- function(fit = NULL, digits = 3, fit.measures = NULL, comp = "standart", transpose = FALSE, type = NULL)
{
    ## Basic Goodness of Fit indices from Klein (2014)
    
    bsic <- c("ntotal", "npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
    rbst <- c('ntotal', 'npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
              'cfi.robust', 'tli.robust', 'rmsea.robust', 'rmsea.ci.lower.robust',
              'rmsea.ci.upper.robust', 'rmsea.pvalue', 'srmr')
    scld <- c('ntotal', 'npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
              'cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled',
              'rmsea.ci.upper.scaled', 'rmsea.pvalue', 'srmr')

    ## Taxonomy from Beaujean (2014)
    
    chi <- list()
    inc <- list()
    par <- list()

    descript <- c("ntotal", "npar") ## hacer el descript opcional mas que obligatorio. 
    chi$dfst <- c("chisq", "df")
    chi$dfsc <- c("chisq.scaled", "df.scaled")
    
    chi$bsic <- c("fmin", "chisq", "df", "pvalue")
    chi$scld <- c("fmin", "chisq.scaled", "df.scaled", "pvalue.scaled")

    inc$bsic <- c("cfi", "nfi", "pnfi", "ifi", "nnfi", "rfi", "tli")
    inc$scld <- c("cfi.scaled", "nfi.scaled", "pnfi", "ifi.scaled", "nnfi.scaled",
                  "rfi.scaled", "tli.scaled")
    inc$rbst <- c("cfi.robust", "nfi.scaled", "pnfi", "ifi.scaled", "nnfi.robust",
                  "rfi.scaled", "tli.robust")

    par$itc  <- c("ecvi", "aic", "bic", "bic2")
    
    par$bsic$ncp <- c("rni", "mfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                      "rmsea.pvalue")
    par$scld$ncp <- c("rni.scaled", "mfi", "rmsea.scaled", "rmsea.ci.lower.scaled",
                      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")
    par$rbst$ncp <- c("rni.robust", "mfi", "rmsea.robust", "rmsea.ci.lower.robust",
                      "rmsea.ci.upper.robust", "rmsea.pvalue.scaled")

    afi <- c("gfi", "agfi", "pgfi", "cn_05", "cn_01", "srmr", "rmr", "rmr_nomean",
             "crmr", "crmr_nomean") ## chisq/df is missing before gfi, It will be later assesed. 


    ## lavaan:::print.fit.measures(fitMeasures(fit)) 

    if (is.list(fit) == "FALSE") ## Cuando fit NO es una lista hacer lo siguiente
    {
        if (is.null(fit.measures) == "FALSE")
        {
            v <- fit.measures
        } else {
            if (inspect(fit, "options")$test == "standard") {
                v <- bsic
            } else {
                v <- rbst
                if (comp == "scaled") {
                    v <- scld
                }
            }
        }

        if (identical(bsic,v) == "TRUE"){
            if (identical(type, "basic") == "TRUE") {
                v <- c(descript, chi$bsic)
            }
            if (identical(type, "inc") == "TRUE") {
                v <- c(descript, inc$bsic)
            }
            if (identical(type, "itc") == "TRUE") {
                v <- c(descript, par$itc)
            }
            if (identical(type, "ncp") == "TRUE") {
                v <- c(descript, par$bsic$ncp)
            }
            if (identical(type, "par") == "TRUE") {
                v <- c(descript, par$itc, par$bsic$ncp)
            }
            if (identical(type, "afi") == "TRUE") {
                v <- c(descript, afi, chi$dfst)
            }
        }

        if (identical(scld,v) == "TRUE") {
            if (identical(type, "basic") == "TRUE") {
                v <- c(descript, chi$scld)
            }
            if (identical(type, "inc") == "TRUE") {
                v <- c(descript, inc$scld)
            }
            if (identical(type, "itc") == "TRUE") {
                v <- c(descript, par$itc)
            }
            if (identical(type, "ncp") == "TRUE") {
                v <- c(descript, par$scld$ncp)
            }
            if (identical(type, "par") == "TRUE") {
                v <- c(descript, par$itc, par$scld$ncp)
            }
            if (identical(type, "afi") == "TRUE") {
                v <- c(descript, afi, chi$dfsc)
            }
            
        }

        if (identical(rbst,v) == "TRUE") {
            if (identical(type, "basic") == "TRUE") {
                v <- c(descript, chi$scld)
            }
            if (identical(type, "inc") == "TRUE") {
                v <- c(descript, inc$rbst)
            }
            if (identical(type, "itc") == "TRUE") {
                v <- c(descript, par$itc)
            }
            if (identical(type, "ncp") == "TRUE") {
                v <- c(descript, par$rbst$ncp)
            }
            if (identical(type, "par") == "TRUE") {
                v <- c(descript, par$itc, par$rbst$ncp)
            }
            if (identical(type, "afi") == "TRUE") {
                v <- c(descript, afi, chi$dfsc)
            }

        }
        
        c <- round(fitMeasures(fit)[v], digits)

        if (is.null(fit.measures) == "FALSE") {
            c <- data.frame(c)
            rownames(c) <- v
            colnames(c) <- deparse(substitute(fit))
            if (transpose == "TRUE") {
                c
            } else {
                t(c)
            }
            
        } else {
            c <- data.frame(c)
            rownames(c) <- v
            colnames(c) <- deparse(substitute(fit))
            if (transpose == "TRUE") {
                c
            } else {
                t(c)
            }
            if (is.null(type) == "TRUE") {
                c <- c(c, c[3]/c[4])
                names(c)[length(c)] <- "chisq/df"
                c <- c[c(1,2,3,4, length(c), 5:(length(c)-1))]
                d <- data.frame(c)
                rownames(d) <- names(c)
                colnames(d) <- deparse(substitute(fit))
                if (transpose == "TRUE") {
                    d
                } else {
                    t(d)
                }
            }
            if (identical(v,c(descript,chi$bsic)) == "TRUE") {
                c <- c(c, c[4]/c[5])
                names(c)[length(c)] <- "chisq/df"
                d <- data.frame(c)
                rownames(d) <- names(c)
                colnames(d) <- deparse(substitute(fit))
                if (transpose == "TRUE") {
                    d
                } else {
                    t(d)
                }
            }
            if (identical(v,c(descript,chi$scld)) == "TRUE") {
                c <- c(c, c[4]/c[5])
                names(c)[length(c)] <- "chisq/df"
                d <- data.frame(c)
                rownames(d) <- names(c)
                colnames(d) <- deparse(substitute(fit))
                if (transpose == "TRUE") {
                    d
                } else {
                    t(d)
                }
            }
            if (identical(v,c(descript,afi, chi$dfst)) == "TRUE") {
                chisq <- c[length(c)-1]/c[length(c)]
                names(chisq) <- "chisq/df"
                c <- c[-((length(c)-1):length(c))]
                c <- c(c, chisq)
                c <- c[c(1,2,length(c), 3:(length(c)-1))]
                d <- data.frame(c)
                rownames(d) <- names(c)
                colnames(d) <- deparse(substitute(fit))
                if (transpose == "TRUE") {
                    d
                } else {
                    t(d)
                }
            }
            if (identical(v,c(descript,afi, chi$dfsc)) == "TRUE") {
                chisq <- c[length(c)-1]/c[length(c)]
                names(chisq) <- "chisq/df"
                c <- c[-((length(c)-1):length(c))]
                c <- c(c, chisq)
                c <- c[c(1,2,length(c), 3:(length(c)-1))]
                d <- data.frame(c)
                rownames(d) <- names(c)
                colnames(d) <- deparse(substitute(fit))
                if (transpose == "TRUE") {
                    d
                } else {
                    t(d)
                }
            }
        }

    } else { ## Cuando Fit SI ES una lista hacer lo siguiente
        if (is.null(fit.measures) == "FALSE") {
            ## Falta ver como hacer para determinar que todos los elementos de la lista si son iguales - sapply. Tambien incluir el pretty.
            ## Codigo posible para ver si todos los elementos de la lista son iguales a standard
            ## a <- sapply(fit, function(x) inspect(x, "options")$test)
            ## if (sum(a == "standard") == length(a)) {"hello"} else {"hola"; if (sum(a == "standard") < 1) {"chau"}}
            v <- fit.measures
        } else {
            if (comp == "standart") {
                v <- bsic
                if (is.null(type) == "TRUE") {
                    v <- bsic
                }
                if (identical(type,"basic") == "TRUE") {
                    v <- c(descript, chi$bsic)
                }
                if (identical(type, "inc") == "TRUE") {
                    v <- c(descript, inc$bsic)
                }
                if (identical(type, "itc") == "TRUE") {
                    v <- c(descript, par$itc)
                }
                if (identical(type, "ncp") == "TRUE") {
                    v <- c(descript, par$bsic$ncp)
                }
                if (identical(type, "par") == "TRUE") {
                    v <- c(descript, par$itc, par$bsic$ncp)
                }
                if (identical(type, "afi") == "TRUE") {
                    v <- c(descript, afi, chi$dfst)
                }
            }
            if (comp == "scaled") {
                v <- scld
                if (is.null(type) == "TRUE") {
                    v <- scld
                }
                if (identical(type, "basic") == "TRUE") {
                    v <- c(descript, chi$scld)
                }
                if (identical(type, "inc") == "TRUE") {
                    v <- c(descript, inc$scld)
                }
                if (identical(type, "itc") == "TRUE") {
                    v <- c(descript, par$itc)
                }
                if (identical(type, "ncp") == "TRUE") {
                    v <- c(descript, par$scld$ncp)
                }
                if (identical(type, "par") == "TRUE") {
                    v <- c(descript, par$itc, par$scld$ncp)
                }
                if (identical(type, "afi") == "TRUE") {
                    v <- c(descript, afi, chi$dfsc)
                }
            }
            if (comp == "robust") {
                v <- rbst
                if (is.null(type) == "TRUE") {
                    v <- rbst
                }
                if (identical(type, "basic") == "TRUE") {
                    v <- c(descript, chi$scld)
                }
                if (identical(type, "inc") == "TRUE") {
                    v <- c(descript, inc$rbst)
                }
                if (identical(type, "itc") == "TRUE") {
                    v <- c(descript, par$itc)
                }
                if (identical(type, "ncp") == "TRUE") {
                    v <- c(descript, par$rbst$ncp)
                }
                if (identical(type, "par") == "TRUE") {
                    v <- c(descript, par$itc, par$rbst$ncp)
                }
                if (identical(type, "afi") == "TRUE") {
                    v <- c(descript, afi, chi$dfsc)
                }
            }
        }        
        
        c <- sapply(fit, function(x) round(fitmeasures(x)[v], digits))

        if (is.null(fit.measures) == "FALSE") {
            c <- data.frame(c)
            rownames(c) <- v
            if (transpose == "TRUE") {
                c
            } else {
                t(c)
            }
            
        } else {
            c <- data.frame(c)
            rownames(c) <- v
            if (transpose == "TRUE") {
                c
            } else {
                t(c)
            }
            if (is.null(type) == "TRUE") {
                rownames(c) <- v
                c <- rbind(c, c[3,]/c[4,])
                rownames(c)[nrow(c)] <- "chisq/df"
                c <- t(t(c[c(1,2,3,4,nrow(d), 5:(nrow(c)-1)),]))
                colnames(c) <- names(fit)
                c <- as.data.frame(c)
                if (transpose == "TRUE") {
                    c
                } else {
                    t(c)
                }
            }
            if (identical (v,c(descript, chi$bsic)) == "TRUE") {
                c <- rbind(c, c[c(nrow(c)-2),]/c[c(nrow(c)-1),])
                rownames(c)[nrow(c)] <- "chisq/df"
                colnames(c) <- names(fit)
                c <- as.data.frame(c)
                if (transpose == "TRUE") {
                    c
                } else {
                    t(c)
                }
            }
            if (identical (v,c(descript, chi$scld)) == "TRUE") {
                c <- rbind(c, c[c(nrow(c)-2),]/c[c(nrow(c)-1),])
                rownames(c)[nrow(c)] <- "chisq/df"
                colnames(c) <- names(fit)
                c <- as.data.frame(c)
                if (transpose == "TRUE") {
                    c
                } else {
                    t(c)
                }
            }
            if (identical (v,c(descript, afi, chi$dfst)) == "TRUE") {
                chisq <- c[c(nrow(c)-1),]/c[nrow(c),]
                chisq <- t(as.data.frame(chisq))
                rownames(chisq) <- "chisq/df"
                c <- rbind(c[1:(nrow(c)-2),],chisq)
                c <- c[c(1,2,nrow(c),3:(nrow(c)-1)),]
                colnames(c) <- names(fit)
                if (transpose == "TRUE") {
                    as.data.frame(c)
                } else {
                    as.data.frame(t(c))
                }
            }
            if (identical (v,c(descript, afi, chi$dfsc)) == "TRUE") {
                chisq <- c[c(nrow(c)-1),]/c[nrow(c),]
                chisq <- t(as.data.frame(chisq))
                rownames(chisq) <- "chisq/df"
                c <- rbind(c[1:(nrow(c)-2),],chisq)
                c <- c[c(1,2,nrow(c),3:(nrow(c)-1)),]
                colnames(c) <- names(fit)
                if (transpose == "TRUE") {
                    as.data.frame(c)
                } else {
                    as.data.frame(t(c))
                }
            }
        }
    }
}

modind <- function(fit, digits) {
    head(modificationindices(fit, power = T, sort = T), digits)
}


###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
core.fit <- function(fit = NULL, digits = 3, fit.measures = NULL, comp = "standart", transpose = FALSE, type = NULL)
{
    bsic <- c("ntotal", "npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
    rbst <- c('ntotal', 'npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
              'cfi.robust', 'tli.robust', 'rmsea.robust', 'rmsea.ci.lower.robust',
              'rmsea.ci.upper.robust', 'rmsea.pvalue', 'srmr')
    scld <- c('ntotal', 'npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
              'cfi.scaled', 'tli.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled',
              'rmsea.ci.upper.scaled', 'rmsea.pvalue', 'srmr')

    ## Taxonomy from Beaujean (2014)
    
    chi <- list()
    inc <- list()
    par <- list()

    descript <- c("ntotal", "npar")
    
    chi$bsic <- c("fmin", "chisq", "df", "pvalue")
    chi$scld <- c("fmin", "chisq.scaled", "df.scaled", "pvalue.scaled")

    inc$bsic <- c("cfi", "nfi", "pnfi", "ifi", "nnfi", "rfi", "tli")
    inc$scld <- c("cfi.scaled", "nfi.scaled", "pnfi", "ifi.scaled", "nnfi.scaled",
                  "rfi.scaled", "tli.scaled")
    inc$rbst <- c("cfi.robust", "nfi.scaled", "pnfi", "ifi.scaled", "nnfi.robust",
                  "rfi.scaled", "tli.robust")

    par$itc  <- c("ecvi", "aic", "bic", "bic2")
    
    par$bsic$ncp <- c("rni", "mfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                      "rmsea.pvalue")
    par$scld$ncp <- c("rni.scaled", "mfi", "rmsea.scaled", "rmsea.ci.lower.scaled",
                      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled")
    par$rbst$ncp <- c("rni.robust", "mfi", "rmsea.robust", "rmsea.ci.lower.robust",
                      "rmsea.ci.upper.robust", "rmsea.pvalue.scaled")

    afi <- c("gfi", "agfi", "pgfi", "cn_05", "cn_01", "srmr", "rmr", "rmr_nomean",
             "crmr", "crmr_nomean") ## chisq/df is missing before gfi, It will be later assesed. 


    ## lavaan:::print.fit.measures(fitMeasures(fit)) 

    if (is.list(fit) == "FALSE") ## Cuando fit NO es una lista hacer lo siguiente
    {
        if (is.null(fit.measures) == "FALSE")
        {
            v <- fit.measures
        } else {
            if (inspect(fit, "options")$test == "standard") {
                v <- bsic
            } else {
                v <- rbst
                if (comp == "scaled") {
                    v <- scld
                }
            }
        }


        c <- round(fitMeasures(fit)[v], digits)

        if (is.null(fit.measures) == "FALSE") {
            c <- data.frame(c)
            rownames(c) <- v
            colnames(c) <- deparse(substitute(fit))
            if (transpose == "TRUE") {
                c
            } else {
                t(c)
            }
            
        } else {
            c <- c(c, c[3]/c[4])
            names(c)[length(c)] <- "chisq/df"
            c <- c[c(1,2,3,4, length(c), 5:(length(c)-1))]
            d <- data.frame(c)
            rownames(d) <- names(c)
            colnames(d) <- deparse(substitute(fit))
            if (transpose == "TRUE") {
                d
            } else {
                t(d)
            }
        }

    } else { ## Cuando Fit SI ES una lista hacer lo siguiente
        if (is.null(fit.measures) == "FALSE") {
            v <- fit.measures
        } else {
            if (comp == "standart") {
                v <- bsic
            }
            if (comp == "scaled") {
                v <- scld
            }
            
            if (comp == "robust") {
                v <- rbst
            }
            
            c <- sapply(fit, function(x) round(fitmeasures(x)[v], digits))

            if (is.null(fit.measures) == "FALSE") {
                c <- data.frame(c)
                rownames(c) <- v
                if (transpose == "TRUE") {
                    c
                } else {
                    t(c)
                }
                
            } else {
                rownames(c) <- v
                c <- rbind(c, c[3,]/c[4,])
                rownames(c)[nrow(c)] <- "chisq/df"
                c <- t(t(c[c(1,2,3,4,nrow(d), 5:(nrow(c)-1)),]))
                colnames(c) <- names(fit)
                if (transpose == "TRUE") {
                    c
                } else {
                    t(c)
                }
            }
    }
}
#################

                                        # Packages

setwd(getwd())
options("scipen"=1000)
options("digits"=4)
options(max.print = 99999)
options("show.signif.stars" = TRUE)
library(semTools)
library(lavaan)
library(semPlot)
library(MVN)

                                        # Useful Lists

model <- list()
fit <- list()
mod_ind <- list()
v <- list()
alfa <- list()

                                        # Useful Function

v$fitindicies <- c("npar", "chisq", "chisq.scaled","df", "df.scaled", "pvalue", "cfi", "cfi.robust","rmsea","rmsea.robust","rmsea.ci.lower", "rmsea.ci.upper", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust","srmr")
                                        # CFA Function's Better Presentation - Goodness of Fit Indicies
core_fitmeasures <- function(fit = fits$n1, fitindicies = v$fitindicies, digits = 3) { x <- fitmeasures(fit)
    round(x[fitindicies], digits)}

sapply(fit, function(x) core_fitmeasures(x))

                                        # Dataset

## base <- haven::read_sav("/Users/mddavila/Desktop/Base de w 2019.sav")
base <- read.csv2("/Users/mddavila/Desktop/Base de w 2019.csv")
base2 <- read.csv2("/Users/mddavila/Desktop/Base W  Rafa SF y SM.csv")
base <- base[,-c(32:40)]
base2 <- base2[,11:22]
base <- cbind(base, base2)
base$Sexo[base$Sexo == 1] <- 0
base$Sexo[base$Sexo == 2] <- 1
base$S1[base$S1 == 0] <- 1
base$S1[base$S1 == 25] <- 2
base$S1[base$S1 == 50] <- 3
base$S1[base$S1 == 75] <- 4
base$S1[base$S1 == 100] <- 5


                                        # Models TOTAL SF1

model$SFTotalOriginal <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
       C =~ C1 + C2 + C3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + a_C_AF*C + a_S_AF*Sexo
     RO ~ a_N_RO*N + a_E_RO*E + a_C_RO*C + a_S_RO*Sexo
     HA ~ a_N_HA*N + a_E_HA*E + a_C_HA*C + a_S_HA*Sexo
     OS ~ a_N_OS*N + a_E_OS*E + a_C_OS*C + a_S_OS*Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO + b_HA_S1*HA + b_OS_S1*OS  + C_S*Sexo

'
fit$m1 <- sem(model$SFTotalOriginal, data = base, estimator = "MLM", orthogonal = T)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)


model$SFTotalTrimming1 <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + Sexo
     RO ~ a_N_RO*N + a_E_RO*E + Sexo
     OS ~ a_N_OS*N + a_E_OS*E + Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO + b_OS_S1*OS + Sexo
     

'
fit$m1 <- sem(model$SFTotalTrimming1, data = base, estimator = "WLS", orthogonal = T)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

                                        # Models Parcial SF1

model$SFParcialOriginal <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
       C =~ C1 + C2 + C3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + a_C_AF*C + a_S_AF*Sexo
     RO ~ a_N_RO*N + a_E_RO*E + a_C_RO*C + a_S_RO*Sexo
     HA ~ a_N_HA*N + a_E_HA*E + a_C_HA*C + a_S_HA*Sexo
     OS ~ a_N_OS*N + a_E_OS*E + a_C_OS*C + a_S_OS*Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO + b_HA_S1*HA + b_OS_S1*OS  + N + E + C + C_S*Sexo
'
fit$m1 <- sem(model$SFParcialOriginal, data = base, estimator = "WLS", orthogonal = T)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

model$SFParcialTrimming1 <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + Sexo
     RO ~ a_N_RO*N + a_E_RO*E + Sexo
     OS ~ a_N_OS*N + a_E_OS*E + Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO+ b_OS_S1*OS  + N + E  + C_S*Sexo

   # 
'
fit$m1 <- sem(model$SFParcialTrimming1, data = base, estimator = "WLS", orthogonal = T)

summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

model$SPParcialTrimming2 <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + Sexo
     RO ~ a_N_RO*N + a_E_RO*E + Sexo
     OS ~ a_N_OS*N + a_E_OS*E + Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO+ b_OS_S1*OS  + N + E  + C_S*Sexo

     N ~~ E
     AF ~~ RO
     AF ~~ OS
     RO ~~ OS
'
fit$m1 <- sem(model$SPParcialTrimming2, data = base, estimator = "WLS", orthogonal = T)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)


                                        # Models Total CSM & CSF


model$SF_SMTotalOriginal <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
       C =~ C1 + C2 + C3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
     # Health
       CSF =~ Ffísica_SF + Rolfísico_SF + Dolor_SF + Saludgnl_SF
       CSM =~ RolemoC_SM + Vitalidad_SM + SaludMental_SM + Fsocial_SM

   # Structural Model
     AF ~ N + E + C + Sexo
     RO ~ N + E + C + Sexo
     HA ~ N + E + C + Sexo
     OS ~ N + E + C + Sexo

     CSF ~ AF + RO + HA + OS + Sexo
     CSM ~ AF + RO + HA + OS + Sexo

   # Covariance
     N ~~ E
     N ~~ C
     E ~~ C
     CSF ~~ CSM
'

fit$m1 <- sem(model$SF_SMTotalOriginal, data = base, estimator = "WLS", orthogonal = T)
fit$m1 <- sem(model$SF_SMTotalOriginal, data = base, estimator = "MLM", orthogonal = T)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)





#################################################################################




model$SFTotalOriginal <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
       C =~ C1 + C2 + C3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E + a_C_AF*C + a_S_AF*Sexo
     RO ~ a_N_RO*N + a_E_RO*E + a_C_RO*C + a_S_RO*Sexo
     HA ~ a_N_HA*N + a_E_HA*E + a_C_HA*C + a_S_HA*Sexo
     OS ~ a_N_OS*N + a_E_OS*E + a_C_OS*C + a_S_OS*Sexo

     S1 ~ b_AF_S1*AF + b_RO_S1*RO + b_HA_S1*HA + b_OS_S1*OS  + C_S*Sexo
     AF ~ Sexo

'


model$m1 <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3
       E =~ E1 + E2 + E3
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S

       Sexo ~ E + N + AF + RO + OS + S1
       SEXO =~ 1*Sexo
       

  # Structural Model
     AF ~ a_N_AF*N + a_E_AF*E
     RO ~ a_N_RO*N + a_E_RO*E
     OS ~ a_N_OS*N + a_E_OS*E

     S1 ~ b_AF_S1*AF + b_RO_S1*RO + b_OS_S1*OS


  # Covariance
    N ~~ E
    AF ~~ RO
    AF ~~ OS
    RO ~~ OS

   S1 ~~ 0*S1
   Sexo ~ 0*Sexo
   Health =~ 1*S1
   

'

fit$m1 <- sem(model$m1, data = base, estimator = "WLS")
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)


### Sorpresa

model$m1 <- '
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3 + N4
       E =~ E1 + E2 + E3 + E4
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
     # Health
       Salud =~ S1
       S1 ~~ 0*S1

  # Structural Model
    Salud ~ AF + RO + OS + Sexo
    AF ~ N + E + Sexo
    RO ~ N + E + Sexo
    OS ~ N + E + Sexo

  # Control Variable
    N ~ Sexo
    E ~ Sexo

  # Correlation
    N  ~~ E
    AF ~~ RO
    AF ~~ OS
    RO ~~ OS

'
fit$m1 <- sem(model$m1, data = base, estimator = "MLM")
fit$m1 <- sem(model$m1, data = base, estimator = "MLM", likelihood = "wishart")
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

semPaths(fit$m1, whatLabels = "std", sizeMan = 3, sizeLat = 5, style = "lisrel", edge.color = "black", layout = "tree3", edge.label.cex = 0.4,layoutSplit = TRUE, optimizeLatRes = TRUE, fixedStyle=1, freeStyle=1, exoVar=FALSE, rotation =1,  levels = c(1,3,6,9,12,15))



model$m1 <- ' ## TOTAL : BUEN MODELO, SE COMPRUEBA LO QUE PIENSA MONICA SI ACEPTA LA EXISTENCIA DE RELACION ENTRE LOS RASGOS DE PERSONALIDAD
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3 + N4
       E =~ E1 + E2 + E3 + E4
       C =~ C1 + C2 + C3 + C4
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
     # Health
       Salud =~ S1
       S1 ~~ 0*S1

  # Structural Model
    AF ~ a01*N + a02*E + a03*C 
    RO ~ a04*N + a05*E + a06*C
    OS ~ a07*N + a08*E + a09*C 
    HA ~ a10*N + a11*E + a12*C

    Salud ~ b1*AF + b2*RO + b3*OS + b4*HA

  # Control Variable
    N + E + C + AF + RO + OS + HA + Salud ~ Sexo

  # Correlation
    N ~~ E
    N ~~ C
    E ~~ C

  # Indirect Effect
    indirect01:= a01*b1
    indirect02:= a04*b2
    indirect03:= a07*b3
    indirect04:= a10*b4

    indirect05:= a02*b1
    indirect06:= a05*b2
    indirect07:= a08*b3
    indirect08:= a11*b4

    indirect09:= a03*b1
    indirect10:= a06*b2
    indirect11:= a09*b3
    indirect12:= a12*b4
'
fit$m1 <- sem(model$m1, data = base, estimator = "ML", test = "Yuan.Bentler", se = "robust.huber.white", likelihood = "wishart", optim.method = "BFGS")

fit$m1 <- sem(model$m1, data = base, estimator = "MLM")
head(modificationindices(fit$m1, power = T, sort = T), 20)

summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

model$m1 <- ' ## MEDIACION PARCIAL BUEN MODELO
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3 + N4
       E =~ E1 + E2 + E3 + E4
       C =~ C1 + C2 + C3 + C4
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
     # Health
       Salud =~ S1
       S1 ~~ 0*S1

  # Structural Model
    Salud ~ AF + RO + OS + HA + N + E + C + Sexo
    AF ~ N + E + C + Sexo
    RO ~ N + E + C + Sexo
    OS ~ N + E + C + Sexo
    HA ~ N + E + C + Sexo

  # Control Variable
    N ~ Sexo
    E ~ Sexo
    C ~ Sexo

  # Correlation
    N ~~ E
    N ~~ C
    E ~~ C
'
fit$m1 <- sem(model$m1, data = base, estimator = "DWLS")

model$m1 <- ' # TOTAL
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3 + N4
       E =~ E1 + E2 + E3 + E4
       C =~ C1 + C2 + C3 + C4
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
     # Health
       CSF =~ Ffísica_SF + Rolfísico_SF + Dolor_SF + Saludgnl_SF
       CSM =~ RolemoC_SM + Vitalidad_SM + SaludMental_SM + Fsocial_SM

  # Structural Model
    CSF ~ AF + RO + OS + HA + Sexo
    CSM ~ AF + RO + OS + HA + Sexo
    AF ~ N + E + C + Sexo
    RO ~ N + E + C + Sexo
    OS ~ N + E + C + Sexo
    HA ~ N + E + C + Sexo

  # Control Variable
    N ~ Sexo
    E ~ Sexo
    C ~ Sexo

  # Correlation
    N ~~ E
    N ~~ C
    E ~~ C
    CSF ~~ CSM
'
fit$m1 <- sem(model$m1, data = base, estimator = "MLM")
head(modificationindices(fit$m1, power = T, sort = T), 60)
summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)

model$m1 <- ' #PARCIAL
  # Measurement Model
     # Personality
       N =~ N1 + N2 + N3 + N4
       E =~ E1 + E2 + E3 + E4
       C =~ C1 + C2 + C3 + C4
     # Health Behaviors
       AF =~ CS1_AF + CS4_AF + CS5_AF + CS6_AF
       RO =~ CS8_RO + CS9_RO + CS10_RO
       OS =~ CS60_OS + CS61_OS + CS62_OS + CS65_OS + CS67_OS + CS69_0S
       HA =~ CS36_HA + CS37_HA + CS39_HA + CS41_HA + CS43_HA + CS48_HA + CS49_HA
     # Health
       CSF =~ Ffísica_SF + Rolfísico_SF + Dolor_SF + Saludgnl_SF
       CSM =~ RolemoC_SM + Vitalidad_SM + SaludMental_SM + Fsocial_SM

  # Structural Model
    CSF ~ AF + RO + OS + HA + N + E + C + Sexo
    CSM ~ AF + RO + OS + HA + N + E + C + Sexo
    AF ~ N + E + C + Sexo
    RO ~ N + E + C + Sexo
    OS ~ N + E + C + Sexo
    HA ~ N + E + C + Sexo

  # Control Variable
    N ~ Sexo
    E ~ Sexo
    C ~ Sexo

  # Correlation
    N ~~ E
    N ~~ C
    E ~~ C
    CSF ~~ CSM
    AF ~~ RO
    AF ~~ OS
    RO ~~ OS
    AF ~~ HA
    RO ~~ HA
    OS ~~ HA
'
    fit$m1 <- sem(model$m1, data = base, estimator = "DWLS")
    ## DWLS N muy pequena para computar Gamma
    head(modificationindices(fit$m1, power = T, sort = T), 60)
    summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)


######################### Path Analysis ########################################

        base <- read.csv2("/Users/mddavila/Desktop/Base para Path 2019.csv")
        colnames(base) <- c("Caso","Sexo", "S1", "CSF", "CSM", "N", "E", "C", "AF", "RO", "HA", "OS")

        model$partial <- '
AF ~ a01*N + a02*E + a03*C
RO ~ a04*N + a05*E + a06*C
OS ~ a07*N + a08*E + a09*C
HA ~ a10*N + a11*E + a12*C

S1 ~ b1*AF + b2*RO + b3*OS + b4*HA
AF + RO + OS + HA + N + E + C + S1 ~ Sexo

'
        fit$m1 <-sem(model$partial, data = base, estimator = "MLM")
        summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)
        head(modificationindices(fit$m1, power = T, sort = T), 20)

        model$total <- '
AF ~ a01*N + a02*E + a03*C
RO ~ a04*N + a05*E + a06*C
OS ~ a07*N + a08*E + a09*C
HA ~ a10*N + a11*E + a12*C

S1 ~ b1*AF + b2*RO + b3*OS + b4*HA + c1*N + c2*E + c3*C
AF + RO + OS + HA + N + E + C + S1 ~ Sexo

'
        fit$m1 <-sem(model$total, data = base, estimator = "MLM")
        summary(fit$m1, standardized = T, fit.measures = T, rsquare = T)
        head(modificationindices(fit$m1, power = T, sort = T), 20)
