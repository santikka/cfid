test_that("type required", {
  expect_error(try_type(x = 3))
})

# Interface ---------------------------------------------------------------

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
g2 <- dag("X -> W -> Y <- Z <- D X <-> Y X -> Y")
v1 <- cf("Y", 0L, c(X = 0L))
v2 <- cf("X", 1L)
v3 <- cf("Z", 0L, c(D = 0L))
v4 <- cf("D", 0L)
c1 <- conj(v1, v2, v3, v4)

test_that("invalid identifiable function inputs fail", {
  expect_error(
    identifiable(),
    "Argument `g` is missing"
  )
  expect_error(
    identifiable(c1),
    "Argument `g` must be a `dag` object"
  )
  expect_error(
    identifiable(g1),
    "Argument `gamma` is missing"
  )
  expect_error(
    identifiable(g1, g1),
    "Unable to coerce `gamma` into a `counterfactual_conjunction` object"
  )
  expect_error(
    identifiable(g1, c1, g1),
    "Unable to coerce `delta` into a `counterfactual_conjunction` object"
  )
  expect_error(
    identifiable(g1, cf("Y")),
    paste0(
      "Argument `gamma` contains counterfactual variables ",
      "without a value assignment"
    )
  )
  expect_error(
    identifiable(g1, conj(cf("Y"))),
    paste0(
      "Argument `gamma` contains counterfactual variables ",
      "without a value assignment"
    )
  )
  expect_error(
    identifiable(g1, cf("Y", 0L), cf("X")),
    paste0(
      "Argument `delta` contains counterfactual variables ",
      "without a value assignment"
    )
  )
  expect_error(
    identifiable(g1, cf("Y", 0L), conj(cf("X"))),
    paste0(
      "Argument `delta` contains counterfactual variables ",
      "without a value assignment"
    )
  )
})

test_that("query printing works", {
  out <- identifiable(g1, c1)
  expect_output(print(out), "The query .+ is identifiable")
  out <- causal_effect(dag("X -> Y"), "Y", "X")
  expect_output(print(out), "The query .+ is identifiable")
  out <- identifiable(g1, conj(v1), cf("Y", 0L, c(Y = 1L)))
  expect_output(print(out), "The query is undefined")
})
