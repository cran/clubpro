set.seed(22)

test_dat <- data.frame(condition = rep(c(1, 2), each = 9),
                       items = c(7,8,5,7,8,7,9,6,7,5,6,4,5,7,6,4,5,5))

test_dat$condition <- factor(test_dat$condition)

m1 <- club(test_dat$items, test_dat$condition, nreps = 1000L)

expect_equal(m1$correct_classifications, 15)
expect_equal(m1$incorrect_classifications, 3)
expect_equal(m1$ambiguous_classifications, 0)
expect_equal(round(m1$pcc, 2), 83.33)
expect_equal(m1$correct_classifications, 15)
expect_equal(m1$incorrect_classifications, 3)
expect_equal(m1$ambiguous_classifications, 0)

expect_identical(clubpro:::to_indicator_matrix(c(1,2,3)), matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, byrow=TRUE))
expect_equal(round(clubpro:::c_pcc(test_dat$items, test_dat$condition, imprecision=0, normalise_cols=TRUE), 2), 83.33)