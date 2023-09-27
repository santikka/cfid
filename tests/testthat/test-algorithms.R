# ID* and IDC* ------------------------------------------------------------

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
g2 <- dag("X -> W -> Y <- Z <- D X <-> Y X -> Y")
g3 <- dag("X -> Y <-> A <-> B <-> Z <- X")
g4 <- dag("C -> A -> Y; C -> Y")

v1 <- cf("Y", 0L, c(X = 0L))
v2 <- cf("X", 1L)
v3 <- cf("Z", 0L, c(D = 0L))
v4 <- cf("D", 0L)
v5 <- cf("Y", 0L, c(X = 0L, Z = 0L))
v6 <- cf("Z", 1L, c(X = 0L))
v7 <- cf("Y", 0L, c(Y = 1L))
v8 <- cf("Y", 0L, c(Y = 0L))
v9 <- cf("A", 0L)
v10 <- cf("B", 0L)
v11 <- cf("Z", 0L, c(X = 1L))
v12 <- cf("Y", 1L, c(X = 1L))
v13 <- cf("W", 0L)
c1 <- conj(v1, v2, v3, v4)

test_that("identifiable conjunction", {
  out <- identifiable(g1, c1)
  expect_true(out$id)
})

test_that("non-identifiable conjunctions", {
  out <- identifiable(g2, c1)
  expect_false(out$id)
  out <- identifiable(g1, c1 + v13)
  expect_false(out$id)
})

test_that("identifiable conditional conjunction", {
  out <- identifiable(g1, conj(v1), conj(v2, v3, v4))
  expect_true(out$id)
})

test_that("non-identifiable conditional conjunction", {
  out <- identifiable(g2, conj(v1), conj(v2, v3, v4))
  expect_false(out$id)
  out <- identifiable(dag("X -> Y"), conj(v1, v12))
  expect_false(out$id)
})

test_that("undefined conditional conjunction", {
  out <- identifiable(g1, conj(v1), conj(v7))
  expect_true(out$undefined)
})

test_that("joint gamma/delta inconsistent", {
  out <- identifiable(g1, conj(v1, v2, v3, v4, v5, v6))
  expect_equal(out$formula$terms[[1L]]$val, 0L)
  out <- identifiable(g1, conj(v1, v2, v3), conj(v4, v5, v6))
  expect_equal(out$formula$terms[[1L]]$val, 0L)
})

test_that("incompatible interventions", {
  out <- identifiable(g3, conj(v1, v9, v10, v11))
  expect_false(out$id)
})

test_that("tautology", {
  out <- identifiable(g1, conj(v8))
  expect_true(out$id)
  expect_equal(out$formula$terms[[1L]]$val, 1L)
})

test_that("inconsistent", {
  out <- identifiable(g1, conj(v7))
  expect_true(out$id)
  expect_equal(out$formula$terms[[1L]]$val, 0L)
})

test_that("remove tautology", {
  out <- identifiable(g1, conj(v1, v2, v3, v3, v8))
  expect_true(out$id)
})

test_that("auto convert singletons", {
  out1 <- identifiable(g1, v1)
  out2 <- identifiable(g1, conj(v1))
  out3 <- identifiable(g1, v1, v2)
  out4 <- identifiable(g1, v1, conj(v2))
  expect_identical(out1, out2)
  expect_identical(out3, out4)
})

test_that("graphs without bidirected edges are supported", {
  expect_error(identifiable(g4, cf("Y", 0, c(A = 1)), cf("A", 0)), NA)
})

test_that("various counterfactuals", {
  h <- dag("X -> Z -> Y X <-> Z")
  w1 <- cf("Y", 1, c(Z = 0))
  w2 <- cf("Z", 0, c(X = 0))
  w3 <- cf("X", 0)
  w1o <- cf("Y", 1)
  w2o <- cf("Z", 0)
  w3o <- cf("X", 1)
  d1 <- conj(w1)
  d2 <- conj(w1, w2, w3)
  d2o <- conj(w1o, w2o, w3o)
  expect_true(identifiable(h, d1)$id)
  expect_true(identifiable(h, d2)$id)
  expect_true(identifiable(h, d2, d2o)$formula$terms[[1]]$val == 0L)
  expect_true(
    identifiable(h, d2, d2o[-3L] + cf("X", 0))$formula$terms[[1]]$val == 1L
  )
})

test_that("length zero delta via recursion", {
  h <- dag("X -> Y <- Z")
  d1 <- conj(cf("Y", 0))
  d2 <- conj(cf("X", 0), cf("Z", 0))
  expect_error(identifiable(h, d1, d2), NA)
})

test_that("inconsistent in counterfactual graph", {
  h <- dag("X -> Z -> Y")
  d1 <- conj(cf("Z", 0), cf("Z", 1, c("X" = 0)))
  out <- identifiable(h, d1)
  expect_true(out$id)
  expect_identical(out$formula$terms[[1]], probability(val = 0L))
})

test_that("inconsistent interventions", {
  h <- dag("X -> Z -> Y")
  d1 <- conj(cf("Z", 0), cf("Z", 1, c("X" = 0)), cf("X", 1))
  expect_false(identifiable(h, d1)$id)
})

test_that("inconsitent within c-component", {
  out <- identifiable(g1, conj(v1, v2, v3, v6))
  expect_true(out$id)
  expect_identical(out$formula$terms[[1]], probability(val = 0L))
})

test_that("nonidentifiable c-component", {
  h <- dag("Z -> X -> Y")
  d1 <- conj(v1, v12, cf("Z", 0L))
  out <- identifiable(h, d1)
  expect_false(out$id)
})

# ID and IDC --------------------------------------------------------------

test_that("bow-arc", {
  g <- dag("X -> Y <-> X")
  out <- causal_effect(g, "Y", "X")
  expect_false(out$id)
})

test_that("backdoor", {
  g <- dag("X -> Y <- Z -> X")
  out <- causal_effect(g, "Y", "X")
  expect_true(out$id)
  expect_identical(
    format(out$formula),
    "\\sum_{z} P(y|z,x)P(z)"
  )
})

test_that("frontdoor", {
  g <- dag("X -> Z -> Y <-> X")
  out <- causal_effect(g, "Y", "X")
  expect_true(out$id)
  expect_identical(
    format(out$formula),
    "\\sum_{z} P(z|x)\\left(\\sum_{x^*} P(y|x^*,z)P(x^*)\\right)"
  )
})

test_that("napkin", {
  g <- dag("W -> Z -> X -> Y <-> W <-> X")
  out <- causal_effect(g, "Y", "X")
  expect_true(out$id)
  expect_identical(
    format(out$formula),
    paste0(
      "\\frac{\\sum_{w} P(y|w,z,x)P(x|w,z)P(w)}",
      "{\\sum_{w,y^*} P(y^*|w,z,x)P(x|w,z)P(w)}"
    )
  )
})

test_that("nonidentifiable napkin variant", {
  g <- dag("W -> Z -> X -> Y; Z -> Y <-> X <-> W")
  out <- causal_effect(g, "Y", "X")
  expect_false(out$id)
})

test_that("identifiable conditional causal effects", {
  g <- dag("A -> {X, Z, B}; X -> Z -> Y; B -> Y; X <-> A <-> Y <-> X <-> B")
  expect_true(causal_effect(g, "Y", "X", "A")$id)
  expect_true(causal_effect(g, "Y", "X", c("A", "B"))$id)
  expect_true(causal_effect(g, "Y", "X", c("A", "B", "Z"))$id)
})

test_that("non-identifiable conditional causal effect", {
  g <- dag("X -> Z -> Y <-> Z <-> X")
  expect_false(causal_effect(g, "Y", "X", "Z")$id)
})

test_that("conditional simplification is carried out", {
  g <- dag("Z <-> X -> Z -> Y")
  out <- causal_effect(g, "Y", "X", "Z")
  expect_true(out$id)
  expect_identical(format(out$formula), "P(y|x,z)")
  g <- dag("X -> Z -> Y")
  out <- causal_effect(g, "Y", "X", "Z")
  expect_true(out$id)
  expect_identical(format(out$formula), "P(y|x,z)")
})

# Identification pipeline -------------------------------------------------

test_that("simple pipeline", {
  g <- dag("X -> Z -> Y <-> Z")
  out1 <- identifiable(
    g,
    cf("Y", 0, c("X" = 0)), cf("Z", 0, c("X" = 0)),
    data = "interventions"
  )
  out2 <- identifiable(
    g,
    cf("Y", 0, c("X" = 0)), cf("Z", 0, c("X" = 0)),
    data = "observations"
  )
  expect_true(out1$id)
  expect_true(out2$id)
  expect_identical(
    format(out1$formula),
    "P_{x}(y|z)"
  )
  expect_identical(
    format(out2$formula),
    "\\frac{P(y|x,z)P(z|x)}{\\sum_{y^*} P(y^*|x,z)P(z|x)}"
  )
})

test_that("nonidentifiable from observations alone", {
  g <- dag("X -> Z -> Y <-> Z <-> X")
  out1 <- identifiable(
    g,
    cf("Y", 0, c("X" = 0)), cf("Z", 0, c("X" = 0)),
    data = "both"
  )
  out2 <- identifiable(
    g,
    cf("Y", 0, c("X" = 0)), cf("Z", 0, c("X" = 0)),
    data = "observations"
  )
  expect_true(out1$id)
  expect_false(out2$id)
  expect_identical(
    format(out1$formula),
    "P_{x}(y|z)"
  )
  idfun <- functional(
    numerator = out1$formula,
    denominator = out1$formula
  )
  out3 <- identify_terms(x = idfun, data = "observations", g = g)
  out4 <- identify_terms(x = idfun, data = "both", g = g)
  expect_false(out3$id)
  expect_true(out4$id)
})

test_that("quotient", {
  out <- identifiable(g1, conj(v1), conj(v2, v3, v4), data = "observations")
  expect_true(out$id)
  expect_identical(
    format(out$formula),
    "\\frac{\\sum_{w} P(w|x)P(y|x',d,w,z)P(x')}{P(x')}",
  )
})

