# clubpro, an R package for classification using binary procrustes rotation.
# Copyright (C) 2023-2024  Timothy Beechey (tim.beechey@proton.me
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' Compare models.
#'
#' @details
#' Compare the PCC of two clubprofit models and compute the chance-value of the
#' difference.
#' @param m1 an object of class "clubprofit" produced by \code{club()}
#' @param m2 an object of class "clubprofit" produced by \code{club()}
#' @return an object of type "clubprocomparison"
#' @examples
#' m1 <- club(width ~ location, jellyfish)
#' m2 <- club(length ~ location, jellyfish)
#' compare(m1, m2)
#' @export
compare <- function(m1, m2) {
    UseMethod("compare")
}


#' @export
compare.default <- function(m1, m2) {
    .NotYetImplemented()
}


#' @export
compare.clubprofit <- function(m1, m2) {
    stopifnot("Models must be fitted with the same number of randomisations"= m1$nreps == m2$nreps)
    pcc_diff <- abs(pcc(m1) - pcc(m2))
    diff_dist <- pcc_replicates(m1) - pcc_replicates(m2)
    cval <- length(abs(diff_dist)[abs(diff_dist) >= pcc_diff]) / m1$nreps
    structure(list(pcc_diff = pcc_diff,
                   diff_dist = diff_dist,
                   cval = cval),
              class = "clubprocomparison")
}


#' Generate a summary of a comparison of clubprofit models.
#' @param object an object of class "clubprocomparison".
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' m1 <- club(width ~ location, jellyfish)
#' m2 <- club(length ~ location, jellyfish)
#' z <- compare(m1, m2)
#' summary(z)
#' @export
summary.clubprocomparison <- function(object, ...) {
    structure(
        list(pcc_diff = object$pcc_diff,
             min_pcc_difference = min(object$diff_dist),
             max_pcc_difference = max(object$diff_dist),
             nreps = length(object$diff_dist),
             cval = object$cval),
        class = "summary.clubprocomparison"
    )
}


#' @export
print.summary.clubprocomparison <- function(x, ..., digits = 2L) {
    stopifnot("digits must be a number"= is.numeric(digits))
    stopifnot("digits cannot be negative"= digits >= 0)
    stopifnot("digits must be a single number"= length(digits) == 1)

    cat("********** Model Comparison **********\n")
    cat("Absolute PCC difference: ", round(x$pcc_diff, 2), "\n")
    cat("Minimum random PCC difference: ", round(x$min_pcc_difference, digits), "\n")
    cat("Maximum random PCC difference: ", round(x$max_pcc_difference, digits), "\n")
    cat("Chance-value: ", round(x$cval, 2), "\n")
}


#' @export
print.clubprocomparison <- function(x, ...) {
    print(summary(x))
}


#' Plot model comparison.
#'
#' @details
#' Plot a distribution of PCCs computed from randomly reordered data
#' used to calculate the chance-value for a model comparison.
#' @param x an object of class "clubprocomparison".
#' @param ... ignored
#' @return no return value, called for side effects only.
#' @examples
#' m1 <- club(width ~ location, jellyfish)
#' m2 <- club(length ~ location, jellyfish)
#' z <- compare(m1, m2)
#' plot(z)
#' @export
plot.clubprocomparison <- function(x, ...) {
    max_x <- min(max(max(x$diff_dist), x$pcc_diff) + 5, 105)
    x_lims <- c(-max_x, max_x)

    histogram(as.vector(x$diff_dist), col = palette()[1], xlab = "PCC",
        xlim = x_lims, ylab = "",
        panel = function(...) {
            panel.histogram(..., border = "white")
            panel.abline(v = c(-x$pcc_diff, x$pcc_diff), col = "red", lty = 2)
        }
    )
}
