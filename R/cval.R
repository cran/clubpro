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



#' Chance value.
#'
#' @details
#' Compute the chance that randomly reordered data results in a
#' percentage of correctly classified observations at least as high as
#' the observed data.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric value.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' cval(mod)
#' @export
cval <- function(m) {
    UseMethod("cval")
}


#' @export
cval.default <- function(m) {
    .NotYetImplemented()
}


#' @export
cval.clubprofit <- function(m) {
    m$cval
}


#' PCC replicates.
#'
#' @details
#' Returns an object containing a vector of PCC replicates used to
#' calculate the chance-value.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an object of class clubprorand.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' head(pcc_replicates(mod))
#' @export
pcc_replicates <- function(m) {
    UseMethod("pcc_replicates")
}


#' @export
pcc_replicates.default <- function(m) {
    .NotYetImplemented()
}


#' @export
pcc_replicates.clubprofit <- function(m) {
    pcc_replicates <- m$pcc_replicates
    attr(pcc_replicates, "observed_pcc") <- m$pcc
    class(pcc_replicates) <- "clubprorand"
    pcc_replicates
}


#' Convert the output of pcc_replicates() to a data.frame.
#'
#' @details
#' This function is useful to format pcc replicates data for plotting.
#' @param x an object of class "clubprorand"
#' @param row.names ignored
#' @param optional ignored
#' @param ... ignored
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- pcc_replicates(mod)
#' as.data.frame(z)
#' @export
as.data.frame.clubprorand <- function(x, row.names = NULL, optional = FALSE, ...) {
    data.frame(rep = seq_along(unclass(x)), pcc = unclass(x))
}


#' Plot PCC replicates.
#'
#' @details
#' Plot the distribution of PCCs computed from randomly reordered data
#' used to calculate the chance-value.
#' @param x an object of class "clubprofit" produced by \code{club()}
#' @param ... ignored
#' @return no return value, called for side effects only.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' plot(pcc_replicates(mod))
#' @export
plot.clubprorand <- function(x, ...) {
    dat <- as.data.frame(x)
    histogram(dat$pcc, type = "count", xlab = "PCC", ylab = "Count", col = palette()[1], xlim = c(0, 100),
        panel = function(...) {
            panel.histogram(..., border = "white")
            panel.abline(v = attr(x, "observed_pcc"), col = "red", lty = 2)
        }
    )
}
