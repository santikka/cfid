# Inputs

test_that("valid inputs", {
  expect_error(probability(val = function(x) x))
  expect_error(probability(var = "X"))
  expect_error(probability(do = "X"))
  expect_error(probability(sumset = "X"))
  expect_error(probability(summand = "X"))
  expect_error(probability(numerator = "X"))
  expect_error(probability(denominator = "X"))
})

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
id1 <- identifiable(g1, conj(v1, v2, v3, v4))
id2 <- identifiable(g1, conj(v1), conj(v2, v3, v4))
prob3 <- id2$prob
prob3$denominator <- probability(val = 1L)

# Formatting

test_that("probability format", {
  expect_identical(
    format(id1$prob),
    "\\sum_{w} p_{x}(w)p_{w,z}(y,x')p_{d}(z)p(d)"
  )
  expect_identical(
    format(id1$prob, use_primes = FALSE),
    "\\sum_{w} p_{x}(w)p_{w,z}(y,x^{(1)})p_{d}(z)p(d)"
  )
  expect_identical(
    format(id2$prob),
    "\\frac{\\sum_{w} p_{x}(w)p_{w,z}(y,x')}{p(x')}"
  )
  expect_identical(
    format(id2$prob, use_do = TRUE),
    "\\frac{\\sum_{w} p(w|do(x))p(y,x'|do(w,z))}{p(x')}"
  )
  expect_identical(
    format(prob3),
    "\\sum_{w} p_{x}(w)p_{w,z}(y,x')"
  )
  expect_identical(
    format(probability(val = 1L)),
    as.character(1L)
  )
})

test_that("probability print", {
  expect_output(print(id1$prob))
})
