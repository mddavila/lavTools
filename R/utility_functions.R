## Mardia Test ========================================================================

mardiaTest <- function(data, digits = 3) {

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
    if (inherits(data, "list") == TRUE) {
        lapply(data, function(x) mardia(x, digits))
    } else {
        mardia(data, digits)
    }
}

## ParTable Est  ========================================================================

parEst <- function(fit = NULL, std = TRUE, simple = TRUE, digits = 4) {

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
#'  The famous Holzinger and Swineford (1939) example
#'  HS.model <- " visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 "
#'
#'  fit <- cfa(HS.model, data=HolzingerSwineford1939)
#'  modind(fit)
#' @aliases modification_index
#' @keywords internal
modind <- function(fit, n = 20) {
    head(lavaan::modificationindices(fit, power = T, sort = T), n)
}
