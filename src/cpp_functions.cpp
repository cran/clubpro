// clubpro, an R package for classification using binary procrustes rotation.
// Copyright (C) 2023  Timothy Beechey (tim.beechey@protonmail.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat to_indicator_matrix(arma::vec v) {
    auto n {v.n_elem};
    auto m {v.max()};
    arma::mat A(n, m);
    for (size_t i {}; i < n; i++) {
        A(i, v[i]-1.0) = 1.0;
    }
    return A;
}

// [[Rcpp::export]]
arma::mat normalise_columns(arma::mat A) {
    for (size_t j {}; j < A.n_cols; j++) {
        auto colfactor = sqrt(accu(square(A.col(j))));
        for (size_t i {}; i < A.n_rows; i++) {
            A(i, j) = colfactor == 0.0 ? 0.0 : A(i, j) / colfactor;
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat normalise_rows(arma::mat A) {
    for (size_t i {}; i < A.n_rows; i++) {
        auto rowfactor = sqrt(accu(square(A.row(i))));
        for (size_t j {}; j < A.n_cols; j++) {
            A(i, j) = rowfactor == 0.0 ? 0.0 : A(i, j) / rowfactor;
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat dichotemise_matrix(arma::mat A) {
    for (size_t i {}; i < A.n_rows; i++) {
        auto m = max(A.row(i));
        for(size_t j {}; j < A.n_cols; j++) {
            A(i, j) = ((A(i, j) == m) && (m > 0)) ? 1.0 : 0.0;
        }
    }
    return A;
}

// [[Rcpp::export]]
arma::mat binary_procrustes_rotation(arma::vec obs, arma::mat target_mat, bool normalise_cols) {
    arma::mat obs_mat = to_indicator_matrix(obs);
    arma::mat T = trans(obs_mat) * target_mat;
    arma::mat A = normalise_cols ? obs_mat * normalise_rows(normalise_columns(T)) : obs_mat * normalise_rows(T);
    return A;
}

// [[Rcpp::export]]
double c_pcc(arma::vec obs, arma::mat target_indicator_mat, int imprecision, bool normalise_cols) {
    arma::mat binary_matrix = dichotemise_matrix(binary_procrustes_rotation(obs, target_indicator_mat, normalise_cols));
    double matches{};
    for (size_t i {}; i < binary_matrix.n_rows; i++) {
        if (accu(binary_matrix.row(i)) == 1.0) {
            if (std::abs(int(binary_matrix.row(i).index_max()) - int(target_indicator_mat.row(i).index_max())) <= imprecision) {
                matches += 1.0;
            } 
        }
    }
    return (matches / obs.n_elem) * 100;
}

// [[Rcpp::export]]
arma::vec shuffle_obs_pccs(arma::vec obs, arma::mat target_indicator_mat, int imprecision, int nreps, bool normalise_cols) {
    arma::vec pccs(nreps);
    for (size_t i {}; i < nreps; i++) {
        Rcpp::checkUserInterrupt();
        pccs(i) = c_pcc(arma::shuffle(obs), target_indicator_mat, imprecision, normalise_cols);
    }
    return pccs;
}

// [[Rcpp::export]]
arma::vec random_dat_pccs(arma::vec obs, arma::mat target_indicator_mat, int imprecision, int nreps, bool normalise_cols) {
    arma::vec pccs(nreps);
    arma::vec obs_range = arma::linspace(min(obs), max(obs), (max(obs) - min(obs)) + 1);
    for (size_t i {}; i < nreps; i++) {
        Rcpp::checkUserInterrupt();
        pccs(i) = c_pcc(Rcpp::RcppArmadillo::sample(obs_range, obs.n_elem, true), target_indicator_mat, imprecision, normalise_cols);
    }
    return pccs;
}
