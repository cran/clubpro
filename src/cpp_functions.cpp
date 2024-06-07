// clubpro, an R package for classification using binary procrustes rotation.
// Copyright (C) 2023-2024  Timothy Beechey (tim.beechey@proton.me)
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
#include <progress.hpp>
#include <progress_bar.hpp>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]


// [[Rcpp::export]]
arma::mat to_indicator_matrix(arma::vec v) {
    auto n {v.n_elem};
    auto m {v.max() + 1};
    arma::mat A(n, m);
    for (size_t i {0}; i < n; i++) {
        A(i, v(i)) = 1.0;
    }
    return A;
}


// [[Rcpp::export]]
arma::mat normalise_columns(arma::mat A) {
    for (size_t j {0}; j < A.n_cols; j++) {
        auto colfactor = std::sqrt(arma::accu(arma::square(A.col(j))));
        for (size_t i {0}; i < A.n_rows; i++) {
            A(i, j) = colfactor == 0.0 ? 0.0 : A(i, j) / colfactor;
        }
    }
    return A;
}


// [[Rcpp::export]]
arma::mat normalise_rows(arma::mat A) {
    for (size_t i {0}; i < A.n_rows; i++) {
        auto rowfactor = std::sqrt(arma::accu(arma::square(A.row(i))));
        for (size_t j {0}; j < A.n_cols; j++) {
            A(i, j) = rowfactor == 0.0 ? 0.0 : A(i, j) / rowfactor;
        }
    }
    return A;
}


// [[Rcpp::export]]
arma::mat dichotemise_matrix(arma::mat A) {
    for (size_t i {0}; i < A.n_rows; i++) {
        auto m = arma::max(A.row(i));
        for(size_t j {0}; j < A.n_cols; j++) {
            A(i, j) = ((A(i, j) == m) && (m > 0)) ? 1.0 : 0.0;
        }
    }
    return A;
}


// [[Rcpp::export]]
arma::mat binary_procrustes_rotation(arma::vec obs, arma::mat target_mat, bool normalise_cols) {
    arma::mat obs_mat = to_indicator_matrix(obs);
    arma::mat T = arma::trans(obs_mat) * target_mat;
    arma::mat A = normalise_cols ? obs_mat * normalise_rows(normalise_columns(T)) : obs_mat * normalise_rows(T);
    return A;
}


// [[Rcpp::export]]
arma::vec row_max(arma::mat X) {
    arma::vec max_vals(X.n_rows);
    for (size_t i {0}; i < X.n_rows; i++) {
        max_vals(i) = arma::max(X.row(i));
    }
    return max_vals;
}


// [[Rcpp::export]]
double c_pcc(arma::vec obs, arma::mat target_indicator_mat, int imprecision, bool normalise_cols) {
    arma::mat binary_matrix = dichotemise_matrix(binary_procrustes_rotation(obs, target_indicator_mat, normalise_cols));
    double matches{0.0};
    for (size_t i {0}; i < binary_matrix.n_rows; i++) {
        if (arma::accu(binary_matrix.row(i)) == 1.0) {
            if (std::abs(int(binary_matrix.row(i).index_max()) - int(target_indicator_mat.row(i).index_max())) <= imprecision) {
                matches += 1.0;
            } 
        }
    }
    return (matches / obs.n_elem) * 100;
}


// [[Rcpp::export]]
arma::vec shuffle_obs_pccs(arma::vec obs, arma::mat target_indicator_mat, int imprecision,
                           size_t nreps, bool normalise_cols, bool display_progress) {
    Progress p(nreps, display_progress);
    arma::vec pccs(nreps);
    for (size_t i {0}; i < nreps; i++) {
        p.increment();
        Rcpp::checkUserInterrupt();
        pccs(i) = c_pcc(arma::shuffle(obs), target_indicator_mat, imprecision, normalise_cols);
    }
    return pccs;
}


// [[Rcpp::export]]
arma::vec random_dat_pccs(arma::vec obs, arma::mat target_indicator_mat, int imprecision,
                          size_t nreps, bool normalise_cols, bool display_progress) {
    Progress p(nreps, display_progress);
    arma::vec pccs(nreps);
    arma::vec obs_range = arma::linspace(min(obs), max(obs), (max(obs) - min(obs)) + 1);
    for (size_t i {0}; i < nreps; i++) {
        p.increment();
        Rcpp::checkUserInterrupt();
        pccs(i) = c_pcc(Rcpp::RcppArmadillo::sample(obs_range, obs.n_elem, true), 
                        target_indicator_mat, imprecision, normalise_cols);
    }
    return pccs;
}
