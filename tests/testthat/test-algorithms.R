# ID* and IDC*

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
g2 <- dag("X -> W -> Y <- Z <- D X <-> Y X -> Y")
g3 <- dag("X -> Y <-> A <-> B <-> Z <- X")
g4 <- dag("C -> A -> Y; C -> Y")

v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
v5 <- cf("Y", 0, c(X = 0, Z = 0))
v6 <- cf("Z", 1, c(X = 0))
v7 <- cf("Y", 0, c(Y = 1))
v8 <- cf("Y", 0, c(Y = 0))
v9 <- cf("A", 0)
v10 <- cf("B", 0)
v11 <- cf("Z", 0, c(X = 1))
c1 <- conj(v1, v2, v3, v4)

test_that("identifiable conjunction", {
  out <- identifiable(g1, c1)
  expect_true(out$id)
})

test_that("non-identifiable conjunction", {
  out <- identifiable(g2, c1)
  expect_false(out$id)
})

test_that("identifiable conditional conjunction", {
  out <- identifiable(g1, conj(v1), conj(v2, v3, v4))
  expect_true(out$id)
})

test_that("non-identifiable conditional conjunction", {
  out <- identifiable(g2, conj(v1), conj(v2, v3, v4))
  expect_false(out$id)
})

test_that("undefined conditional conjunction", {
  out <- identifiable(g1, conj(v1), conj(v7))
  expect_true(out$undefined)
})

test_that("joint gamma/delta inconsistent", {
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

test_that("no bidirected allowed", {
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
  expect_true(identifiable(h, d1)$id) # Identifiable
  expect_true(identifiable(h, d2)$id) # Identifiable
  expect_true(identifiable(h, d2, d2o)$formula$terms[[1]]$val == 0L) # Inconsistent
  expect_true(
    identifiable(h, d2, d2o[-3L] + cf("X", 0))$formula$terms[[1]]$val == 1L
  ) # Trivial
})

test_that("length zero delta via recursion", {
  h <- dag("X -> Y <- Z")
  d1 <- conj(cf("Y", 0))
  d2 <- conj(cf("X", 0), cf("Z", 0))
  expect_error(identifiable(h, d1, d2), NA)
})

# Interface

test_that("valid input", {
  expect_error(identifiable())
  expect_error(identifiable(c1))
  expect_error(identifiable(g1))
  expect_error(identifiable(g1, g1))
  expect_error(identifiable(g1, c1, g1))
  expect_error(identifiable(g1, cf("Y")))
  expect_error(identifiable(g1, conj(cf("Y"))))
  expect_error(identifiable(g1, cf("Y", 0), cf("X")))
  expect_error(identifiable(g1, cf("Y", 0), conj(cf("X"))))
})
