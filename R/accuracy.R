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



#' Classification accuracy for each observation.
#'
#' @details
#' Returns a character vector containing a string corresponding to each
#' observation indicating whether classification of that observation was
#' "correct", "incorrect", or "ambigous".
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a table
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' accuracy(mod)
#' @export
accuracy <- function(m) {
    UseMethod("accuracy")
}


#' @export
accuracy.default <- function(m) {
    .NotYetImplemented()
}


#' @export
accuracy.clubprofit <- function(m) {
    x <- individual_results(m)
    acc_tab <- table(x$target, x$accuracy)
    class(acc_tab) <- "clubproaccuracy"
    acc_tab
}


#' Plot accuracy.
#'
#' @details
#' Produces a mosaic plot of predictio naccuracy by category
#'
#' @param x an object of class "clubproaccuracy"
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- accuracy(mod)
#' plot(z)
#' @export
plot.clubproaccuracy <- function(x, ...) {
    mosaicplot(x, color = palette(), main = NULL,
               xlab = "Observed Category", ylab = "Prediction Accuracy",
               las = 1, cex.axis = 1)
}


#' @export
print.clubproaccuracy <- function(x, ...) {
    print(unclass(x))
}
