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



#' Predicted category for each observation.
#'
#' @details
#' Returns a character vector containing the name of the predicted category
#' for each observed value.
#' @param object an object of class "clubprofit" produced by \code{club()}
#' @param ... ignored
#' @return a table
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' predict(mod)
#' @export
predict.clubprofit <- function(object, ...) {
    x <- individual_results(object)
    pred_tab <- table(x$target, x$prediction)
    class(pred_tab) <- "clubpropredictions"
    pred_tab
}


#' Plot predictions.
#'
#' @details
#' Produces a mosaic plot of observed versus predicted categories
#'
#' @param x an object of class "clubpropredictions"
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- predict(mod)
#' plot(z)
#' @export
plot.clubpropredictions <- function(x, ...) {
    mosaicplot(x, color = palette(), main = NULL,
               xlab = "Predicted Category", ylab = "Observed Category",
               las = 1, cex.axis = 1)
}


#' @export
print.clubpropredictions <- function(x, ...) {
    print(unclass(x))
}
