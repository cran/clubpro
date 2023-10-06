# clubpro, an R package for classification using binary procrustes rotation.
# Copyright (C) 2023  Timothy Beechey (tim.beechey@protonmail.com)
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

#' Print the model call.
#' @param x an object of class "clubprofit"
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- club(a, b)
#' print(mod)
#' @export
print.clubprofit <- function(x, ...) {
  print(x$call)
}

#' Print a summary of results from a fitted clubpro model.
#' @param object an object of class "clubprofit".
#' @param digits an integer used for rounding numeric values in the output.
#' @param ... ignored
#' @return No return value, called for side effects.
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- club(a, b)
#' summary(mod)
#' summary(mod, digits = 3)
#' @export
summary.clubprofit <- function(object, ..., digits = 2L) {

  # check function arguments
  stopifnot("digits must be a number"= is.numeric(digits))
  stopifnot("digits cannot be negative"= digits >= 0)
  stopifnot("digits must be a single number"= length(digits) == 1)

  cat("********** Classification Results **********\n")
  cat("Observations:", length(object$y), "\n")
  cat("Missing observations:", sum(is.na(object$y)), "\n")
  cat("Target groups:", nlevels(object$x), "\n")
  cat("Correctly classified observations:", object$correct_classifications, "\n")
  cat("Incorrectly classified observations:", object$incorrect_classifications, "\n")
  cat("Ambiguously classified observations:", object$ambiguous_classifications, "\n")
  cat("PCC:", round(object$pcc, digits), "\n")
  cat("Median classification strength index:", round(object$median_csi, digits), "\n\n")
  cat("********** Randomisation Test **********\n")
  cat("Random reorderings:", object$nreps, "\n")
  cat("Minimum random PCC:", round(min(object$pcc_replicates), digits), "\n")
  cat("Maximum random PCC:", round(max(object$pcc_replicates), digits), "\n")
  cat("Chance-value:", round(object$cval, 2), "\n\n")
  print(individual_results(object))
}

#' Individual level classification results.
#'
#' @details
#' Returns a data.frame containing predicted classifications and classification
#' accuracy for each individual observation.
#' @param m an object of class "clubprofit" produced by \code{classify()}
#' @param digits an integer
#' @return a data.frame containing a columns of predictions and prediction
#' accuracy
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- club(a, b)
#' individual_results(mod)
#' individual_results(mod, digits = 3)
#' @export
individual_results <- function(m, digits) {
  UseMethod("individual_results")
}

#' @export
individual_results.default <- function(m, digits) .NotYetImplemented()

#' @export
individual_results.clubprofit <- function(m, digits = 2L) {
  df <- data.frame(individual = 1:length(m$y),
                   observation = m$y,
                   target = m$x,
                   prediction = m$prediction,
                   accuracy = m$accuracy,
                   csi = round(m$csi, 2))
  df$target <- factor(df$target, levels = levels(m$x))
  df$accuracy <- factor(df$accuracy, levels = c("correct", "incorrect", "ambiguous"))
  df
}

#' Plot classification accuracy.
#'
#' @details
#' Produces bar plot showing counts of individuals against observed values within
#' each target grouping. Fill colours indicate whether each individual was
#' classified correctly, incorrectly or ambiguously.
#' @param x an object of class "clubprofit" produced by \code{classify()}
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- club(a, b)
#' plot(mod)
#' @export
plot.clubprofit <- function(x, ...) {
  accuracy <- observation <- target <- NULL
  z <- individual_results(x)
  xlabs <- levels(addNA(z$observation))
  xlabs[is.na(xlabs)] <- "NA"

  if (any(is.na(z$target))) {
    z$target <- addNA(z$target)
    npanels <- nlevels(z$target)
  } else {
    z$target <- factor(z$target)
    npanels <- nlevels(z$target)
  }

  if (any(is.na(z$observation))) {
    z$observation <- addNA(z$observation)
  } else {
    z$observation <- factor(z$observation)
  }


  histogram(~ observation | target,
     data = z,
     type = "count",
     xlim = c(0:(nlevels(z$observation) + 1)),
     scales = list(x = list(at = 1:nlevels(z$observation), labels = xlabs)),
     groups = factor(accuracy, levels = c("correct", "incorrect", "ambiguous")),
     panel = function(...) panel.superpose(..., panel.groups=panel.histogram,
                                           col=c("#0072B2", "#E69F00", "#999999"),
                                           alpha = 0.8, border = "white"),
     xlab = "Observed Value", ylab = "N Individuals",
     layout = c(1, npanels),
     par.settings = list(superpose.polygon = list(col = c("#0072B2", "#E69F00", "#999999"), 
                                                  border="white", alpha = 0.8)),
     auto.key=list(space = "top", rectangles=TRUE, columns = 3)
     )
}
