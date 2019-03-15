## Wald Test  ========================================================================

WaldTest <- function(fit = NULL, digits = 3, cons = c("==0"), order = TRUE,
                     only_trim = FALSE, full = FALSE, keep_label = FALSE,
                     remove_def = TRUE, all = FALSE) {
    ## Piece of code slightly modified from the lavTestWald function in lavaan.
    ## CON <- attr(lavaan::lavParseModelString(cons), "constraints")
    ## LIST <- data.frame(unlist(lapply(CON, "[[", "lhs")),
    ##                    unlist(lapply(CON, "[[", "op")),
    ##                    unlist(lapply(CON, "[[", "rhs")))
    ## colnames(LIST) <- c("lhs", "op", "rhs")
    ## LIST[LIST == ""] <- NA;; sum(LIST == "")
    ## !complete.cases(LIST)
    nlabel <- fit@ParTable$label[fit@ParTable$label != ""]
    back <- fit@ParTable
    if (length(nlabel) == 0 | all == TRUE) {
        par_table <- as.data.frame(fit@ParTable, stringsAsFactors = FALSE)
        free_nr <- (par_table$lhs == par_table$rhs) + (par_table$free < 1)
        par_table$label[free_nr == 0] <- paste0(
            "l", 1:length(par_table$label[free_nr == 0]))
        fit@ParTable$label <- par_table$label
        par_est <- lavaan::parameterEstimates(fit, standardized = TRUE,
                                              remove.def = remove_def)
        par_est  <- par_est[!par_est$label == "", ]
        characters <- par_est[, 1:4]
        numbers <- par_est[, 5:ncol(par_est)]
    } else {
        par_est <- lavaan::parameterEstimates(fit, standardized = TRUE,
                                              remove.def = remove_def)
        par_est <- par_est[!par_est$label == "", ]
        characters <- par_est[, 1:4]
        numbers <- par_est[, 5:ncol(par_est)]
    }
    if (identical(cons, "==0")) {
        label <- par_est$label
        con <- paste0(label, cons)
    } else {
        label <- par_est$label
        con <- paste0(label, cons)
    }
    wald <-  sapply(con, lavaan::lavTestWald, object = fit)
    dframe <- data.frame(matrix(unlist(wald), nrow = ncol(wald), byrow = T),
                       stringsAsFactors = FALSE)
    ## Define whats is going to be in the table
    const    <- colnames(wald)
    stat     <- round(as.numeric(dframe$X1), digits)
    df       <- as.numeric(dframe$X2)
    wald.pv  <- round(as.numeric(dframe$X3), digits)
    type.se  <- dframe$X4
    ## Define the data frame to be created
    out <- data.frame(characters, const, stat, df, wald.pv, type.se, numbers)
    out[, -c(1:5, 9)] <- round(out[, -c(1:5, 9)], digits)
    ## keep label
    if (keep_label == FALSE) {
        fit@ParTable <- back
    }
    ## Full
    if (full == FALSE) {
        out <- out[, 1:9]
    }
    ## Order
    if (order == TRUE) {
        out <- out[order(out$stat), ]
    }
    ## Trimming
    if (only_trim == TRUE) {
        out <- subset(out, wald.pv >= .05)
    }
    out
}

## Mardia Test ========================================================================

mardiaTest <- function(data, digits = 3, pretty = TRUE) {
    mardia <- function(data, digits) {
        skew <- semTools::mardiaSkew(data)
        kurtosis <- semTools::mardiaKurtosis(data)
        kurtosis[4] <- kurtosis[3]
        kurtosis[3] <- ""
        kurtosis <- as.numeric(kurtosis)
        s_k <- as.data.frame(rbind(skew, kurtosis))
        colnames(s_k) <- c(abbreviate("multivariate", 5),
                           "statistic", "df", "p.value")
        s_k <- round(s_k, digits)
        s_k$result <-  ifelse(s_k$p.value > 0.05, "Yes", "No")
        s_k
    }
    if (inherits(data, "data.frame") == TRUE) {
        mardia_test <- mardia(data, digits)
    } else if (inherits(data, "lavaan") == TRUE) {
        mardia_test <- mardia(lavaan::lavInspect(data, "data"), digits)
    } else if (inherits(data, "list") == TRUE &
               sum(sapply(data, class) == "lavaan") == length(data)) {
        mardia_test <- lapply(data, function(x)
            mardia(lavaan::lavInspect(x, "data"), digits))
    } else if (inherits(data, "list") == TRUE &
               sum(sapply(data, class) == "data.frame") == length(data)) {
        mardia_test <- lapply(data, function(x)
            mardia(x, digits))
    }
    if (pretty == TRUE & class(mardia_test) == "data.frame") {
        mardia_test[is.na(mardia_test)] <- ""
    }
    if (pretty == TRUE & class(mardia_test) == "list") {
        pretty_list <- function(ugly_list) {
            ugly_list[is.na(ugly_list)] <- ""
            ugly_list
        }
        mardia_test <- lapply(mardia_test, function(x) pretty_list(x))
    }
    mardia_test
}

## ParTable Est  ========================================================================

parEst <- function(fit = NULL, std = TRUE, simple = TRUE, digits = 4, pretty = TRUE) {
    par_es <- lavaan::parameterEstimates(fit, standardized = std)

    if (simple == TRUE) {
        par_subset <- par_es[par_es$op == "=~", ]
        h2 <- par_subset$std.all^2
        u2 <- 1 - h2
        h_u <- data.frame(h2, u2) ## h2 - Communality; u2 - Uniqueness
        par <- merge(data.frame(par_es, row.names = NULL),
                     data.frame(h_u, row.names = NULL),
                     by = 0, all = TRUE, sort = F)
        par <-  par[order(as.numeric(par[, 1])), ][-1]
        par[, -c(1:3)] <- round(par[, -c(1:3)], digits)
        rownames(par) <- 1:nrow(par)
        par
    } else {
        par_subset <- par_es
        h2 <- par_subset$std.all^2
        u2 <- 1 - h2
        par <- cbind(par_subset, h2, u2)
        par[, -c(1:3)] <- round(par[, -c(1:3)], digits)
    }
    if (pretty == TRUE) {
        par[is.na(par)] <- ""
        par
    } else {
        par
    }
}

## Mod Indices  ========================================================================

#' Summary of Modification Indices
#'
#' A modification index data.frame sort by power.
#'
#' This is a wrapper function.
#' 
#' @param fit A fitted object made in lavaan
#' @param n The first number of elements of the modification index data frame.
#'
#' @return A data frame with the modification indices.
#' @seealso \code{\link[lavaan]{modificationIndices}}
#' @examples
#'  ## The famous Holzinger and Swineford (1939) example
#'     HS.model <- " visual  =~ x1 + x2 + x3
#'                   textual =~ x4 + x5 + x6
#'                   speed   =~ x7 + x8 + x9 "
#'
#'     fit <- cfa(HS.model, data=HolzingerSwineford1939)
#'     modind(fit)
#' @aliases modification_index
#' @keywords internal
#' @import lavaan semTools
#' @importFrom utils head
#' @export
modind <- function(fit, n = 20) {
    head(lavaan::modificationindices(fit, power = T, sort = T), n)
}
