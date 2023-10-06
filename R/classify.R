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


classify <- function(obs, target, imprecision, normalise_cols) {
  unique_group_names <- levels(target)
  predicted_classification <- character(length(obs))
  classification_result <- character(length(obs))
  all_group_names <- character(length(obs))
  target_indicator_mat <- to_indicator_matrix(target)
    
  conformed_mat <- binary_procrustes_rotation(obs, target_indicator_mat, normalise_cols)
  csi <- apply(conformed_mat, 1, max)
  median_csi <- median(csi)
  binary_matrix <- dichotemise_matrix(conformed_mat)
  matches <- 0
    
  for (i in 1:length(all_group_names)) {
    all_group_names[i] <- unique_group_names[target[i]]
  }
    
  for (i in 1:nrow(binary_matrix)) {
    matched_groups <- c()
    for (j in 1:ncol(binary_matrix)) {
      if (binary_matrix[i,j] == 1) {
        matched_groups <- c(matched_groups, unique_group_names[j])
        if (predicted_classification[i] == "") {
          predicted_classification[i] <- unique_group_names[j]
        } else {
            predicted_classification[i] <- paste0(c(predicted_classification[i], unique_group_names[j]), collapse = "|")
        }
      }
    }
    if (sum(binary_matrix[i,]) == 1) {
      if (abs(which.max(binary_matrix[i,]) - which.max(target_indicator_mat[i,])) <= imprecision) {
        matches <- matches + 1
          classification_result[i] <- "correct"
      } else {
          classification_result[i] <- "incorrect"
      }
    }
    else if (sum(binary_matrix[i,]) > 1) {
      if (all_group_names[i] %in% matched_groups) {
        classification_result[i] <- "ambiguous"
      } else {
        classification_result[i] <- "incorrect"
      }
    }
  }
  pcc <- (matches / length(obs)) * 100
  list(predicted_classification = predicted_classification,
       classification_result = classification_result,
       csi = csi,
       median_csi = median_csi,
       pcc = pcc)
}

#' Classify observations
#'
#' \code{club()} is used to classify obervations using binary procrustes
#' rotation.
#' @param y a factor vector of observations.
#' @param x a factor vector of target groups.
#' @param imprecision a number indicting the margin of imprecision allowed in classification.
#' @param nreps the number of replicates to use in the randomisation test.
#' @param normalise_cols a boolean indicating whether to normalise matrix columns.
#' @param reorder_obs a string indicating the method for reordering observations to calculate c-values.
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
#' a <- sample(1:5, 20, replace = TRUE)
#' b <- rep(c("group1", "group2"), each = 10)
#' b <- factor(b)
#' mod <- club(a, b)
#' mod <- club(a, b, nreps = 200L)
#' @export
club <- function(y, x, imprecision = 0, nreps = 1000L, normalise_cols = TRUE, reorder_obs = "shuffle") {

  stopifnot("The second argument to club() must be a vector"=is.null(dim(x))) # is not not a df or matrix
  stopifnot("The second argument to club() must be a vector, not a list"=is.recursive(x) == FALSE) # x is not a list
  stopifnot("The first argument to club() must be a vector, not a list"=is.recursive(y) == FALSE) # y is not a list
  stopifnot("The first argument to club() cannot be a factor"=!is.factor(y))
  stopifnot("The second argument to club() must be a factor"=is.factor(x))
  stopifnot("length of vectors passed to club() are not equal"=length(x) == length(y))
  stopifnot("nreps must be a number"=is.numeric(nreps)) # TRUE for int or double
  stopifnot("nreps must be a positive number"=nreps >= 1) # nreps must be a positve number
  stopifnot("nreps must be a single number"=length(nreps) == 1) # nreps is a single value
  stopifnot("nreps must be a whole number"=nreps %% 1 == 0)
  stopifnot("reorder_obs must be 'shuffle' or 'random'"=reorder_obs %in% c("shuffle", "random"))

  if (is.character(y)) {
    if (any(is.na(y))) {
      obs_num <- as.integer(addNA(y))
    } else {
      obs_num <- as.integer(factor(y))
    }
  } else if (is.numeric(y)) {
    if (any(is.na(y))) {
      obs_num <- as.integer(addNA(y))
    } else {
       obs_num <- as.integer(factor(y))
    }
  }

  x_mat <- to_indicator_matrix(x)

  obs_pcc <- classify(obs_num, x, imprecision, normalise_cols)
  correct_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "correct"])
  ambiguous_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "ambiguous"])
  incorrect_classifications <- length(obs_pcc$classification_result[obs_pcc$classification_result == "incorrect"])
  if (reorder_obs == "shuffle") {
      rand_pccs <- shuffle_obs_pccs(obs_num, x_mat, imprecision, nreps, normalise_cols)
  } else if (reorder_obs == "random") {
      rand_pccs <- random_dat_pccs(obs_num, x_mat, imprecision, nreps, normalise_cols)
  }
  cval <- length(rand_pccs[rand_pccs >= obs_pcc$pcc])/nreps
  return(
    structure(
      list(
        prediction = obs_pcc$predicted_classification,
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
        call = match.call()
      ),
      class = "clubprofit"
    )
  )
}
