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

g2 <- dag("X -> {Z, Y}; Y -> Z")
v5 <- cf("Y", obs = 0L, sub = c(X = 0L))
v6 <- cf("Z", obs = 0L, sub = c(X = 0L))
v7 <- cf("Z", obs = 0L)
id3 <- identifiable(g2, v5, v6)
id4 <- identifiable(g2, v5, v6, data = "obs")
id5 <- identifiable(g2, v5, v7)
id6 <- identifiable(g2, v5, v7, data = "obs")
id7 <- identifiable(g1, conj(v1, v2, v3))

g3 <- dag("X -> Z -> Y")
v8 <- cf("Z", 0, c("X" = 0))
v9 <- cf("Y", 1)
id8 <- identifiable(g3, v8, v9)
id9 <- identifiable(g3, v8, v9, data = "obs")

# Format ------------------------------------------------------------------

test_that("probability format works", {
  expect_identical(
    format(id1$formula),
    "\\sum_{w} P_{x}(w)P_{w,z}(y,x')P_{d}(z)P(d)"
  )
  expect_identical(
    format(id1$formula, use_primes = FALSE),
    "\\sum_{w} P_{x}(w)P_{w,z}(y,x^{(1)})P_{d}(z)P(d)"
  )
  expect_identical(
    format(id2$formula),
    "\\frac{\\sum_{w} P_{x}(w)P_{w,z}(y,x')}{P(x')}"
  )
  expect_identical(
    format(id2$formula, use_do = TRUE),
    "\\frac{\\sum_{w} P(w|do(x))P(y,x'|do(w,z))}{P(x')}"
  )
  expect_identical(
    format(prob3),
    "\\sum_{w} P_{x}(w)P_{w,z}(y,x')"
  )
  expect_identical(
    format(probability(val = 1L)),
    as.character(1L)
  )
  expect_identical(
    format(id3$formula),
    "\\frac{P_{x,y}(z)P_{x}(y)}{\\sum_{y^*} P_{x,y^*}(z)P_{x}(y^*)}"
  )
  expect_identical(
    format(id4$formula),
    "\\frac{P(z|x,y)P(y|x)}{\\sum_{y^*} P(z|x,y^*)P(y^*|x)}"
  )
  # expect_identical(
  #   format(id5$formula),
  #   "\\frac{\\sum_{x^*} P_{x}(y)P(x^*)P_{x^*,y}(z)}{\\sum_{x^*,y^*} P(x^*)P_{x^*,y^*}(z)P_{x^*}(y^*)}"
  # )
  # expect_identical(
  #   format(id6$formula),
  #   "\\frac{\\sum_{x^*} P(y|x)P(x^*)P(z|x^*,y)}{\\sum_{x^*,y^*} P(x^*)P(z|x^*,y^*)P(y^*|x^*)}"
  # )
  # expect_identical(
  #   format(id7$formula),
  #   "\\sum_{w,d^*} P_{x}(w)P_{w,z}(y,x')P_{d}(z)P(d^*)"
  # )
  # expect_identical(
  #   format(id8$formula),
  #   "\\frac{\\sum_{x^*} P_{x}(z)P(x^*)P_{z}(y')}{\\sum_{x^*,z^*} P(x^*)P_{x^*}(z^*)P_{z^*}(y')}"
  # )
  # expect_identical(
  #   format(id9$formula),
  #   "\\frac{\\sum_{x^*} P(z|x)P(x^*)P(y'|x,z)}{\\sum_{x^*,z^*} P(x^*)P(z^*|x^*)P(y'|x,z^*)}"
  # )
})

test_that("probability printing works", {
  expect_output(
    print(id1$formula),
    paste0(
      "\\\\sum_\\{w\\} P_\\{x\\}\\(w\\)P_\\{w,z\\}",
      "\\(y,x'\\)P_\\{d\\}\\(z\\)P\\(d\\)"
    )
  )
  prob <- probability(var = list(cf("X", 0)), cond = list(cf("Z", 0)))
  expect_output(print(prob), "P\\(x|z\\)")
})
