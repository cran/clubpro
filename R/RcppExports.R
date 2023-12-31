# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

to_indicator_matrix <- function(v) {
    .Call(`_clubpro_to_indicator_matrix`, v)
}

normalise_columns <- function(A) {
    .Call(`_clubpro_normalise_columns`, A)
}

normalise_rows <- function(A) {
    .Call(`_clubpro_normalise_rows`, A)
}

dichotemise_matrix <- function(A) {
    .Call(`_clubpro_dichotemise_matrix`, A)
}

binary_procrustes_rotation <- function(obs, target_mat, normalise_cols) {
    .Call(`_clubpro_binary_procrustes_rotation`, obs, target_mat, normalise_cols)
}

c_pcc <- function(obs, target_indicator_mat, imprecision, normalise_cols) {
    .Call(`_clubpro_c_pcc`, obs, target_indicator_mat, imprecision, normalise_cols)
}

shuffle_obs_pccs <- function(obs, target_indicator_mat, imprecision, nreps, normalise_cols) {
    .Call(`_clubpro_shuffle_obs_pccs`, obs, target_indicator_mat, imprecision, nreps, normalise_cols)
}

random_dat_pccs <- function(obs, target_indicator_mat, imprecision, nreps, normalise_cols) {
    .Call(`_clubpro_random_dat_pccs`, obs, target_indicator_mat, imprecision, nreps, normalise_cols)
}

