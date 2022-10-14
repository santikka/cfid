# Inputs ------------------------------------------------------------------

test_that("invalid probability/functional inputs fail", {
  expect_error(probability(val = function(x) x))
  expect_error(probability(var = "X"))
  expect_error(probability(do = "X"))
  expect_error(probability(cond = "X"))
  expect_error(functional(sumset = "X"))
  expect_error(functional(numerator = "X"))
  expect_error(functional(denominator = "X"))
})

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
id1 <- identifiable(g1, conj(v1, v2, v3, v4))
id2 <- identifiable(g1, conj(v1), conj(v2, v3, v4))
prob3 <- id2$formula
prob3$denominator <- probability(val = 1L)

# Format ------------------------------------------------------------------

test_that("probability format works", {
  expect_identical(
    format(id1$formula),
    "\\sum_{w} p_{x}(w)p_{w,z}(y,x')p_{d}(z)p(d)"
  )
  expect_identical(
    format(id1$formula, use_primes = FALSE),
    "\\sum_{w} p_{x}(w)p_{w,z}(y,x^{(1)})p_{d}(z)p(d)"
  )
  expect_identical(
    format(id2$formula),
    "\\frac{\\sum_{w} p_{x}(w)p_{w,z}(y,x')}{p(x')}"
  )
  expect_identical(
    format(id2$formula, use_do = TRUE),
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

test_that("probability printing works", {
  expect_output(
    print(id1$formula),
    paste0(
      "\\\\sum_\\{w\\} p_\\{x\\}\\(w\\)p_\\{w,z\\}",
      "\\(y,x'\\)p_\\{d\\}\\(z\\)p\\(d\\)"
    )
  )
  prob <- probability(var = list(cf("X", 0)), cond = list(cf("Z", 0)))
  expect_output(print(prob), "p\\(x|z\\)")
})
