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



#' Classification strength indices.
#'
#' @details
#' Returns a vector containing the classification strength
#' index for each observation.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' csi(mod)
#' @export
csi <- function(m) {
    UseMethod("csi")
}


#' @export
csi.default <- function(m) {
    .NotYetImplemented()
}


#' @export
csi.clubprofit <- function(m) {
    x <- m$csi
    class(x) <- "clubprocsi"
    x
}


#' Median classification strength index.
#'
#' @details
#' Returns the median classification strength index.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric vector.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' median_csi(mod)
#' @export
median_csi <- function(m) {
    UseMethod("median_csi")
}


#' @export
median_csi.default <- function(m) {
    .NotYetImplemented()
}


#' @export
median_csi.clubprofit <- function(m) {
    m$median_csi
}


#' Convert the output of csi() to a data.frame.
#'
#' @details
#' This function is useful to format pcc replicates data for plotting.
#' @param x an object of class "clubprocsi"
#' @param row.names ignored
#' @param optional ignored
#' @param ... ignored
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- csi(mod)
#' as.data.frame(z)
#' @export
as.data.frame.clubprocsi <- function(x, row.names = NULL, optional = FALSE, ...) {
    data.frame(y = seq_along(unclass(x)), x = unclass(x))
}


#' Plot classification strength indices.
#'
#' @details
#' Produces dotplot showing classification strength for each individual.
#'
#' @param x an object of class "clubprocsi"
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- csi(mod)
#' plot(z)
#' @export
plot.clubprocsi <- function(x, ...) {
    dat <- as.data.frame(x)
    dotplot(y ~ x, dat, pch = 3, lty = 3, col = palette()[1],
            col.line = "grey", cex = 1,
            xlab = "Classification Strength",
            ylab = "Individual")
}
