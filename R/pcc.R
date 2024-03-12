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



#' Percentage of correct classifications.
#'
#' @details
#' Returns the percentage of correctly classified observations.
#' @param m an object of class "clubprofit" produced by \code{club()}
#' @return a numeric value.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' pcc(mod)
#' @export
pcc <- function(m) {
    UseMethod("pcc")
}


#' @export
pcc.default <- function(m) {
    .NotYetImplemented()
}


#' @export
pcc.clubprofit <- function(m) {
    m$pcc
}