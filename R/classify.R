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


classify <- function(obs, target, imprecision, normalise_cols, display_progress) {
    predicted_classification <- character(length(obs))
    classification_result <- character(length(obs))
    target_indicator_mat <- to_indicator_matrix(target)

    conformed_mat <- binary_procrustes_rotation(obs, target_indicator_mat, normalise_cols)
    csi <- row_max(conformed_mat)
    median_csi <- median(csi)
    binary_matrix <- dichotemise_matrix(conformed_mat)
    matches <- 0

    for (i in seq_len(dim(binary_matrix)[1])) {
        predicted_classification[i] <- paste0(levels(target)[(which(binary_matrix[i, ] == 1)) - 1], collapse = "|")

        if (sum(binary_matrix[i, ]) == 1) {
            if (abs(which.max(binary_matrix[i, ]) - which.max(target_indicator_mat[i, ])) <= imprecision) {
                matches <- matches + 1
                classification_result[i] <- "correct"
            } else {
                classification_result[i] <- "incorrect"
            }
        } else if (sum(binary_matrix[i, ]) > 1) {
            classification_result[i] <- "ambiguous"
        }
    }

    pcc <- (matches / length(obs)) * 100

    list(predicted_classification = predicted_classification,
         classification_result = classification_result,
         csi = c(csi),
         median_csi = median_csi,
         pcc = pcc)
}


parse_formula <- function(f, dat) {
    my_cols <- list()
    vars <- all.vars(f)
    preds <- vars[-1]
    for (i in seq(preds)) {
        my_cols[[i]] <- dat[, preds[i]]
    }
    y <- unname(unlist(dat[vars[1]]))
    x <- interaction(my_cols, sep = ":")
    list(y = y, x = x)
}


#' Classify observations.
#'
#' \code{club()} is used to classify obervations using binary procrustes
#' rotation.
#' @param f a formula.
#' @param data a data.frame.
#' @param imprecision a number indicting the margin of imprecision allowed in classification.
#' @param nreps the number of replicates to use in the randomisation test.
#' @param normalise_cols a boolean indicating whether to normalise matrix columns.
#' @param reorder_obs a string indicating the method for reordering observations to calculate c-values.
#' @param display_progress a boolean indictaing whether a progress bar should be displayed.
#' @return an object of class "clubprofit" is a list containing the folllowing
#' components:
#' \describe{
#'   \item{prediction}{a character vector of predicted classifications.}
#'   \item{accuracy}{a character vector indicating whether each classification
#'   is "correct", "incorrect", or "ambiguous".}
#'   \item{pcc}{the percentage of correct classifications.}
#'   \item{cval}{the chance of randomly reordered data producing a PCC >= the
#'   observed PCC.}
#'   \item{pcc_replicates}{a vector of PCCs generated from randomly reordered
#'   data used to calculate \code{cval}.}
#'   \item{call}{the matched call.}
#'   }
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' @export
club <- function(f, data, imprecision, nreps, normalise_cols, reorder_obs, display_progress) {
    UseMethod("club")
}


#' @export
club.default <- function(f, data, imprecision, nreps, normalise_cols, reorder_obs, display_progress) {
    .NotYetImplemented()
}


#' @export
club.formula <- function(f, data, imprecision = 0, nreps = 1000L, normalise_cols = TRUE,
                         reorder_obs = "shuffle", display_progress = FALSE) {

    stopifnot("The data source must be specified"=is.data.frame(data))
    stopifnot("nreps must be a number"=is.numeric(nreps)) # TRUE for int or double
    stopifnot("nreps must be a positive number"=nreps >= 1) # nreps must be a positve number
    stopifnot("nreps must be a single number"=length(nreps) == 1) # nreps is a single value
    stopifnot("nreps must be a whole number"=nreps %% 1 == 0)
    stopifnot("reorder_obs must be 'shuffle' or 'random'"=reorder_obs %in% c("shuffle", "random"))

    vars <- parse_formula(f, data)
    y <- vars$y
    x <- vars$x

    if (is.character(y)) {
        if (any(is.na(y))) {
            obs_num <- as.integer(addNA(y))
        } else {
            obs_num <- as.integer(factor(y))
        }
    } else if (any(is.na(y))) {
        obs_num <- addNA(y)
    } else {
        obs_num <- factor(y)
    }

    x_mat <- to_indicator_matrix(x)

    obs_pcc <- classify(obs_num, x, imprecision, normalise_cols, display_progress)
    correct_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "correct"])
    ambiguous_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "ambiguous"])
    incorrect_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "incorrect"])
    if (reorder_obs == "shuffle") {
        rand_pccs <- shuffle_obs_pccs(obs_num, x_mat, imprecision, nreps, normalise_cols, display_progress)
    } else if (reorder_obs == "random") {
        rand_pccs <- random_dat_pccs(obs_num, x_mat, imprecision, nreps, normalise_cols, display_progress)
    }
    cval <- length(rand_pccs[rand_pccs >= obs_pcc$pcc]) / nreps
    return(structure(list(prediction = obs_pcc$predicted_classification,
                          accuracy = obs_pcc$classification_result,
                          pcc = obs_pcc$pcc,
                          correct_classifications = correct_classifications,
                          ambiguous_classifications = ambiguous_classifications,
                          incorrect_classifications = incorrect_classifications,
                          csi = obs_pcc$csi,
                          median_csi = obs_pcc$median_csi,
                          cval = cval,
                          pcc_replicates = rand_pccs,
                          y = y,
                          x = x,
                          y_num = obs_num,
                          nreps = nreps,
                          imprecision = imprecision,
                          normalise_cols = normalise_cols,
                          call = match.call()),
                     class = "clubprofit"))
}


#' @export
print.clubprofit <- function(x, ...) {
    print(x$call)
}


#' Plot classification accuracy.
#'
#' @details
#' Produces bar plot showing counts of individuals against observed values within
#' each target grouping. Fill colours indicate whether each individual was
#' classified correctly, incorrectly or ambiguously.
#'
#' @param x an object of class "clubprofit" produced by \code{club()}
#' @param ... ignored
#' @return called for side-effects only
#' @examples
#' mod <- club(rate ~ dose, data = caffeine)
#' plot(mod)
#' @export
plot.clubprofit <- function(x, ...) {
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


    histogram(~ observation | target, data = z, type = "count",
              xlim = c(0:(nlevels(z$observation) + 1)),
              scales = list(x = list(at = 1:nlevels(z$observation), labels = xlabs)),
              groups = factor(accuracy, levels = c("correct", "incorrect", "ambiguous")),
              panel = function(...) {
                  panel.superpose(..., panel.groups = panel.histogram,
                                  col = palette()[1:3],
                                  border = "white", alpha = 1.0)},
              xlab = "Observed Value", ylab = "Count",
              layout = c(1, npanels),
              par.settings = list(superpose.polygon = list(col = palette()[1:3], border = "white", alpha = 1.0)),
              auto.key = list(space = "top", rectangles = TRUE, columns = 3))
}
