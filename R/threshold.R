# clubpro, an R package for classification using binary procrustes rotation.
# Copyright (C) 2023-2024  Timothy Beechey (tim.beechey@proton.me)
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
#' @return an object of class clubprothreshold
#' @examples
#' mod <- club(width ~ location, data = jellyfish)
#' threshold(mod)
#' @export
threshold <- function(m) {
    UseMethod("threshold")
}


#' @export
threshold.default <- function(m) {
    .NotYetImplemented()
}


#' @export
threshold.clubprofit <- function(m) {
    obs <- m$y
    target <- m$x
    unique_obs <- sort(unique(obs))
    cutpoint_pccs <- numeric(length(unique_obs) - 1)
    threshold_obs <- rep(1L, length(obs))

    for (i in seq_len(length(cutpoint_pccs))) {
        threshold_obs[which(obs <= unique_obs[i])]  <-  0L
        cutpoint_pccs[i] <- c_pcc(factor(threshold_obs), to_indicator_matrix(target), m$imprecision, m$normalise_cols)
    }

    structure(data.frame(unique_obs = unique_obs[-length(unique_obs)],
                         cutpoint_pccs = cutpoint_pccs),
              class = "clubprothreshold")
}


#' Plot PCC as a function of binary category boundary location.
#'
#' @details
#' Produces an xyplot showing the PCC returned for each possible category boundary.
#'
#' @param x an object of class "clubprothreshold"
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' z <- threshold(mod)
#' plot(z)
#' @export
plot.clubprothreshold <- function(x, ...) {
    xyplot(cutpoint_pccs ~ unique_obs, x, ylim = c(0, 100),
           xlab = "Category Boundary", y = "PCC", type = c("p", "l"), lty = 3,
           cex = 1, pch = 1, col = palette()[1],
           scales = list(x = list(at = x$unique_obs, labels = x$unique_obs)))
}


#' @export
print.clubprothreshold <- function(x, ...) {
    print(data.frame(obs = x$unique_obs, PCC = x$cutpoint_pccs))
}
