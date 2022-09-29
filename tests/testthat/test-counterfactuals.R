# Counterfactual variables

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
  expect_identical(
    format(cf("Y", 1, c("X" = 1)), use_primes = FALSE),
    "y^{(1)}_{x^{(1)}}"
  )
})

test_that("variable print", {
  expect_output(print(cf("Y")))
})

# Counterfactual conjunctions

v1 <- cf("Y", 0)
v2 <- cf("X", 0)
c1 <- conj(v1, v2)

test_that("all variables counterfactual", {
  expect_error(conj(v1, "X"))
})

test_that("list coerce", {
  expect_identical(as.counterfactual_conjunction(list(v1, v2)), c1)
})

test_that("default coerce", {
  expect_identical(as.counterfactual_conjunction(c1), c1)
  expect_error(as.counterfactual_conjunction(0L))
})

test_that("conjunction format", {
  expect_identical(format(c1), "y \u2227 x")
})

test_that("conjunction print", {
  expect_output(print(c1))
})

test_that("arithmetic", {
  expect_identical(v1 + v2, c1)
  expect_identical(conj(v1) + v2, c1)
  expect_identical(v1 + conj(v2), c1)
  expect_identical(conj(v1) + conj(v2), c1)
  expect_identical(v1 + v1, conj(v1))
  expect_identical(c1 + v1, c1)
  expect_identical(v1 + c1, c1)
  expect_error(v1 + "Y")
  expect_error(c1 + "Y")
  expect_error(`+.counterfactual_conjunction`("X", "Y"))
})

# Test conflicts
test_that("produces conflicts", {
  v1 <- cf("Y", 0, c(X = 0))
  v2 <- cf("Y", 1, c(X = 0))
  v3 <- cf("Y", 0, c(X = 1))
  v4 <- cf("Y", 1, c(X = 1))
  expect_length(trivial_conflict(v1, list(v2, v3)), 1)
  expect_length(trivial_conflicts(list(v1, v2, v3, v4)), 2)
  expect_error(check_conflicts(v1, list(v2, v3)))
})

# Value extraction
test_that("both obs and int values received", {
  v1 <- cf("Y", 0, c(X = 1, Z = 0))
  v2 <- cf("X", 1, c(W = 2, Z = 0))
  v3 <- list(var = "X", int = c(W = 1))
  expect_identical(
    evs(list(v1, v2, v3)),
    list(
      c(Y = 0L, X = 1L, Z = 0L),
      c(X = 1L, W = 2L, Z = 0L),
      c(W = 1)
    )
  )
})
