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



#' Generate a summary of results from a fitted clubpro model.
#' @param object an object of class "clubprofit".
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' summary(mod)
#' @export
summary.clubprofit <- function(object, ...) {
    structure(
        list(
            n_obs = length(object$y),
            missing_obs = sum(is.na(object$y)),
            n_groups = nlevels(object$x),
            n_correct = object$correct_classifications,
            n_incorrect = object$incorrect_classifications,
            n_ambiguous = object$ambiguous_classifications,
            pcc = object$pcc,
            median_csi = object$median_csi,
            nreps = object$nreps,
            min_random_pcc = min(object$pcc_replicates),
            max_random_pcc = max(object$pcc_replicates),
            cval = object$cval
        ), class = "summary.clubprofit"
    )
}


#' @export
print.summary.clubprofit <- function(x, ..., digits = 2L) {
    # check function arguments
    stopifnot("digits must be a number"= is.numeric(digits))
    stopifnot("digits cannot be negative"= digits >= 0)
    stopifnot("digits must be a single number"= length(digits) == 1)

    cat("********** Model Summary **********\n\n")
    cat("----- Classification Results -----\n")
    cat("Observations: ", x$nobs, "\n")
    cat("Missing observations: ", x$missing_obs, "\n")
    cat("Target groups: ", x$n_groups, "\n")
    cat("Correctly classified observations: ", x$n_correct, "\n")
    cat("Incorrectly classified observations: ", x$n_incorrect, "\n")
    cat("Ambiguously classified observations: ", x$n_ambiguous, "\n")
    cat("PCC: ", round(x$pcc, digits), "\n")
    cat("Median classification strength index: ", round(x$median_csi, digits), "\n\n")
    cat("----- Randomisation Test Results -----\n")
    cat("Random reorderings: ", x$nreps, "\n")
    cat("Minimum random PCC: ", round(x$min_random_pcc, digits), "\n")
    cat("Maximum random PCC: ", round(x$max_random_pcc, digits), "\n")
    cat("Chance-value: ", round(x$cval, 2), "\n")
}
