set.seed(22)

#################### model with integer data and single predictor ####################
test_dat_int <- data.frame(condition = rep(c(1L, 2L), each = 9),
                       gender = c(1L,2L,2L,1L,2L,1L,1L,1L,2L,1L,1L,1L,2L,2L,2L,2L,2L,1L),
                       items = c(7L,8L,5L,7L,8L,7L,9L,6L,7L,5L,6L,4L,5L,7L,6L,4L,5L,5L),
                       items2 = c(6L,7L,5L,6L,8L,5L,7L,5L,7L,3L,4L,3L,5L,5L,3L,3L,3L,2L))

m1 <- club(items ~ condition, test_dat_int, nreps = 1000L)

expect_error(club(items ~ condition, test_dat_int, nreps = -1000L))
expect_error(club(items ~ condition, test_dat_int, nreps = 0L))
expect_error(club(items ~ condition))
expect_error(club(test_dat_int))

# check types
expect_inherits(m1, "clubprofit")
expect_inherits(individual_results(m1), "data.frame")
expect_inherits(pcc_replicates(m1), "clubprorand")
expect_inherits(predict(m1), "clubpropredictions")
expect_inherits(csi(m1), "clubprocsi")
expect_inherits(accuracy(m1), "clubproaccuracy")

# check getters
expect_equal(n_correct(m1), 15)
expect_equal(n_incorrect(m1), 3)
expect_equal(n_ambiguous(m1), 0)
expect_equal(unclass(predict(m1))[1:4], c(7, 1, 2, 8))
expect_equal(sum(n_correct(m1), n_incorrect(m1), n_ambiguous(m1)), nrow(test_dat_int))
expect_equal(round(pcc(m1), 2), 83.33)
expect_true(cval(m1) < 0.2)
expect_equal(length(csi(m1)), nrow(test_dat_int))
expect_equal(round(unclass(csi(m1)), 2), c(0.97, 1.00, 0.97, 0.97, 1.00, 0.97, 1.00, 0.89, 0.97, 0.97, 0.89,
                                           1.00, 0.97, 0.97, 0.89, 1.00, 0.97, 0.97))
expect_equal(round(median_csi(m1), 2), 0.97)
expect_equal(sum(predict(m1)), nrow(test_dat_int))
expect_equal(sum(unclass(accuracy(m1))), nrow(test_dat_int))

# check functions called for side-effects
expect_stdout(print(m1))
expect_silent(plot(m1))
expect_silent(plot(pcc_replicates(m1)))
expect_stdout(summary(m1))
expect_silent(summary(m1))



#################### model with integer data and crossed predictors ####################
m2 <- club(items ~ condition * gender, test_dat_int, nreps = 1000L)

# check types
expect_inherits(m2, "clubprofit")
expect_inherits(individual_results(m2), "data.frame")
expect_inherits(pcc_replicates(m2), "clubprorand")
expect_inherits(predict(m2), "clubpropredictions")

# check getters
expect_equal(n_correct(m2), 10)
expect_equal(n_incorrect(m2), 8)
expect_equal(n_ambiguous(m2), 0)
expect_equal(sum(n_correct(m2), n_incorrect(m2), n_ambiguous(m2)), nrow(test_dat_int))
expect_equal(round(pcc(m2), 2), 55.56)
expect_true(cval(m2) < 0.5)
expect_true(cval(m2) > 0.2)
expect_equal(length(csi(m2)), nrow(test_dat_int))
expect_equal(round(unclass(csi(m2)), 2), c(0.85, 1.00, 0.69, 0.85, 1.00, 0.85, 1.00, 0.65, 0.85, 0.69, 
                                  0.65, 0.73, 0.69, 0.85, 0.65, 0.73, 0.69, 0.69))
expect_equal(round(median_csi(m2), 2), 0.73)
expect_equal(unclass(predict(m2))[1:12], c(4, 0, 1, 1, 0, 0, 2, 0, 1, 4, 1, 4))
expect_equal(sum(predict(m2)), nrow(test_dat_int))
expect_equal(sum(unclass(accuracy(m2))), nrow(test_dat_int))

# check functions called for side-effects
expect_stdout(print(m2))
expect_silent(plot(m2))
expect_silent(plot(pcc_replicates(m2)))
expect_stdout(summary(m2))
expect_silent(summary(m2))


#################### model with missing data ####################
test_dat_nas <- test_dat_int
test_dat_nas$items[c(3, 13, 17)] <- NA
test_dat_nas$items2[c(2, 7)] <- NA
test_dat_nas$gender[c(6, 10)] <- NA

test_dat_nas$gender <- addNA(test_dat_nas$gender)

m3 <- club(items ~ condition, test_dat_nas, nreps = 1000L)

# check types
expect_inherits(m3, "clubprofit")
expect_inherits(individual_results(m3), "data.frame")
expect_inherits(pcc_replicates(m3), "clubprorand")

# check getters
expect_equal(n_correct(m3), 15)
expect_equal(n_incorrect(m3), 3)
expect_equal(n_ambiguous(m3), 0)
expect_equal(sum(n_correct(m3), n_incorrect(m3), n_ambiguous(m3)), nrow(test_dat_nas))
expect_equal(round(pcc(m3), 2), 83.33)
expect_true(cval(m3) < 0.2)
expect_true(cval(m3) > 0.05)
expect_equal(length(csi(m3)), nrow(test_dat_nas))
expect_equal(round(unclass(csi(m3)), 2), c(0.96,1.00,0.92,0.96,1.00,0.96,1.00,0.92,
                                  0.96,1.00,0.92,1.00,0.92,0.96,0.92,1.00,0.92,1.00))
expect_equal(round(median_csi(m3), 2), 0.96)
expect_equal(sum(predict(m3)), nrow(test_dat_nas))
expect_equal(sum(unclass(accuracy(m3))), nrow(test_dat_nas))

# check functions called for side-effects
expect_stdout(print(m3))
expect_silent(plot(m3))
expect_silent(plot(pcc_replicates(m3)))
expect_stdout(summary(m3))
expect_silent(summary(m3))


#################### model with character data ####################
test_dat_char <- data.frame(condition = rep(c("music", "noise"), each = 9),
                       gender = c("m","f","f","m","f","m","m","m","f","m","m","m","f","f","f","f","f","m"),
                       items = c("g","h","e","g","h","g","i","f","g","e","f","d","e","g","f","d","e","e"))

m4 <- club(items ~ condition * gender, test_dat_char, nreps = 1000L)

# check types
expect_inherits(m4, "clubprofit")
expect_inherits(individual_results(m4), "data.frame")
expect_inherits(pcc_replicates(m4), "clubprorand")

# check getters
expect_equal(n_correct(m4), 10)
expect_equal(n_incorrect(m4), 8)
expect_equal(n_ambiguous(m4), 0)
expect_equal(sum(n_correct(m4), n_incorrect(m4), n_ambiguous(m4)), nrow(test_dat_char))
expect_equal(round(pcc(m4), 2), 55.56)
expect_true(cval(m4) < 0.5)
expect_true(cval(m4) > 0.2)
expect_equal(length(csi(m4)), nrow(test_dat_char))
expect_equal(round(unclass(csi(m4)), 2), c(0.85, 1.00, 0.69, 0.85, 1.00, 0.85, 1.00, 0.65, 0.85, 0.69, 
                                  0.65, 0.73, 0.69, 0.85, 0.65, 0.73, 0.69, 0.69))
expect_equal(round(median_csi(m4), 2), 0.73)
expect_equal(sum(predict(m4)), nrow(test_dat_char))
expect_equal(sum(unclass(accuracy(m4))), nrow(test_dat_char))

# check functions called for side-effects
expect_stdout(print(m4))
expect_silent(plot(m4))
expect_silent(plot(pcc_replicates(m4)))
expect_stdout(summary(m4))
expect_silent(summary(m4))


#################### model with factor data ####################
test_dat_fac <- data.frame(condition = rep(c("music", "noise"), each = 9),
                       gender = c("m","f","f","m","f","m","m","m","f","m","m","m","f","f","f","f","f","m"),
                       items = c("g","h","e","g","h","g","i","f","g","e","f","d","e","g","f","d","e","e"))

test_dat_fac$condition <- factor(test_dat_fac$condition)
test_dat_fac$gender <- factor(test_dat_fac$gender)
test_dat_fac$items <- factor(test_dat_fac$items)

m5 <- club(items ~ condition * gender, test_dat_fac, nreps = 1000L)

# check types
expect_inherits(m5, "clubprofit")
expect_inherits(individual_results(m5), "data.frame")
expect_inherits(pcc_replicates(m5), "clubprorand")

# check getters
expect_equal(n_correct(m5), 10)
expect_equal(n_incorrect(m5), 8)
expect_equal(n_ambiguous(m5), 0)
expect_equal(sum(n_correct(m5), n_incorrect(m5), n_ambiguous(m5)), nrow(test_dat_fac))
expect_equal(round(pcc(m5), 2), 55.56)
expect_true(cval(m5) < 0.5)
expect_true(cval(m5) > 0.2)
expect_equal(length(csi(m5)), nrow(test_dat_fac))
expect_equal(round(unclass(csi(m5)), 2), c(0.85, 1.00, 0.69, 0.85, 1.00, 0.85, 1.00, 0.65, 0.85, 0.69, 
                                  0.65, 0.73, 0.69, 0.85, 0.65, 0.73, 0.69, 0.69))
expect_equal(round(median_csi(m5), 2), 0.73)
expect_equal(sum(predict(m5)), nrow(test_dat_fac))
expect_equal(sum(unclass(accuracy(m5))), nrow(test_dat_fac))

# check functions called for side-effects
expect_stdout(print(m5))
expect_silent(plot(m5))
expect_silent(plot(pcc_replicates(m5)))
expect_stdout(summary(m5))
expect_silent(summary(m5))


# model with floating point data
test_dat_float <- data.frame(condition = rep(c(1, 2), each = 9),
                       gender = c(1,2,2,1,2,1,1,1,2,1,1,1,2,2,2,2,2,1),
                       items = c(7.7,8.8,5.5,7.7,8.8,7.7,9.9,6.6,7.7,5.5,6.6,4.4,5.5,7.7,6.6,4.4,5.5,5.5))
                       
m6 <- club(items ~ condition, test_dat_float, nreps = 1000L)

# check types
expect_inherits(m6, "clubprofit")
expect_inherits(individual_results(m6), "data.frame")
expect_inherits(pcc_replicates(m6), "clubprorand")

# check getters
expect_equal(n_correct(m6), 15)
expect_equal(n_incorrect(m6), 3)
expect_equal(n_ambiguous(m6), 0)
expect_equal(sum(n_correct(m6), n_incorrect(m6), n_ambiguous(m6)), nrow(test_dat_float))
expect_equal(round(pcc(m6), 2), 83.33)
expect_true(cval(m6) < 0.2)
expect_equal(length(csi(m6)), nrow(test_dat_float))
expect_equal(round(unclass(csi(m6)), 2), c(0.97, 1.00, 0.97, 0.97, 1.00, 0.97, 1.00, 0.89, 0.97, 0.97, 0.89, 
                                  1.00, 0.97, 0.97, 0.89, 1.00, 0.97, 0.97))
expect_equal(round(median_csi(m6), 2), 0.97)
expect_equal(sum(predict(m6)), nrow(test_dat_float))
expect_equal(sum(unclass(accuracy(m6))), nrow(test_dat_float))

# check functions called for side-effects
expect_stdout(print(m6))
expect_silent(plot(m6))
expect_silent(plot(pcc_replicates(m6)))
expect_stdout(summary(m6))
expect_silent(summary(m6))


# non-exported functions
expect_identical(clubpro:::to_indicator_matrix(c(1,2,3)), matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, byrow=TRUE))

expect_equal(round(clubpro:::c_pcc(test_dat_int$items, clubpro:::to_indicator_matrix(test_dat_int$condition), 
                   imprecision = 0, normalise_cols = TRUE), 2), 83.33)
