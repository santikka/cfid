# Tests for counterfactual variables and conjunctions

test_that("named interventions", {
    expect_error(cf("Y", 0, c(0)))
})

test_that("variable format", {
    expect_identical(format(cf("Y")), "Y")
    expect_identical(format(cf("Y", 0)), "y")
    expect_identical(format(cf("Y", 0, c("X" = 0))), "y_{x}")
    expect_identical(format(cf("Y", 1, c("X" = 0))), "y'_{x}")
    expect_identical(format(cf("Y", int = c("X" = 0))), "Y_{x}")
    expect_identical(format(cf("Y", int = c("X" = 1))), "Y_{x'}")
    expect_identical(format(cf("Y", 1, c("X" = 1)), use_primes = FALSE), "y^{(1)}_{x^{(1)}}")
})

test_that("variable print", {
    expect_output(print(cf("Y")))
})
