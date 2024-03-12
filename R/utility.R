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



#' Individual level classification results.
#'
#' @details
#' Returns a data.frame containing predicted classifications and classification
#' accuracy for each individual observation.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @param digits an integer
#' @return a data.frame containing a columns of predictions and prediction
#' accuracy
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' individual_results(mod)
#' @export
individual_results <- function(m, digits) {
    UseMethod("individual_results")
}


#' @export
individual_results.default <- function(m, digits) {
    .NotYetImplemented()
}


#' @export
individual_results.clubprofit <- function(m, digits = 2L) {
    df <- data.frame(individual = seq_along(m$y),
                     observation = m$y,
                     target = m$x,
                     prediction = m$prediction,
                     accuracy = m$accuracy,
                     csi = round(m$csi, 2))
    df$target <- factor(df$target, levels = levels(m$x))
    df$accuracy <- factor(df$accuracy, levels = c("correct", "incorrect", "ambiguous"))
    df
}


#' Number of correct classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified correctly by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_correct(mod)
#' @export
n_correct <- function(m) {
    UseMethod("n_correct")
}


#' @export
n_correct.default <- function(m) {
    .NotYetImplemented()
}


#' @export
n_correct.clubprofit <- function(m) {
    m$correct_classifications
}


#' Number of incorrect classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified incorrectly by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_incorrect(mod)
#' @export
n_incorrect <- function(m) {
    UseMethod("n_incorrect")
}


#' @export
n_incorrect.default <- function(m) {
    .NotYetImplemented()
}


#' @export
n_incorrect.clubprofit <- function(m) {
    m$incorrect_classifications
}


#' Number of ambiguous classifications.
#'
#' @details
#' Returns the number of observations which were
#' classified ambiguously by the model.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return an integer.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' n_ambiguous(mod)
#' @export
n_ambiguous <- function(m) {
    UseMethod("n_ambiguous")
}


#' @export
n_ambiguous.default <- function(m) {
    .NotYetImplemented()
}


#' @export
n_ambiguous.clubprofit <- function(m) {
    m$ambiguous_classifications
}


# Clean up C++ when package is unloaded.
.onUnload <- function(libpath) {
    library.dynam.unload("clubpro", libpath)
}
