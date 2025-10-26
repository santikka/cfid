# DAGs --------------------------------------------------------------------

test_that("no self-loops or cycles", {
  expect_error(dag("X -> X"))
  expect_error(dag("X -> Z -> X"))
  expect_error(dag("X -> Z -> Y -> X"))
})

test_that("extra delimiter invariance", {
  expect_identical(dag("X -> Y -> Z"), dag("X -> Y; Y -> Z"))
  expect_identical(dag("X -> Y -> Z"), dag("X -> Y\n Y -> Z"))
  expect_identical(dag("{X, Y, Z}"), dag("X Y Z"))
  expect_identical(dag("{{X, Y, Z}}"), dag("X Y Z"))
  expect_identical(dag("{{X, Y} Z}"), dag("X Y Z"))
})

test_that("zero edges is allowed", {
  expect_length(attr(dag("X Y Z"), "labels"), 3)
  expect_length(attr(dag("{X Y Z}"), "labels"), 3)
})

test_that("duplicate edge invariance", {
  expect_identical(dag("X -> Y <- X"), dag("X -> Y"))
  expect_identical(dag("X <-> Y <-> X"), dag("X <-> Y"))
})

test_that("singleton bidirected", {
  expect_error(dag("X <-> X"))
})

test_that("syntax errors fail", {
  expect_error(dag("->"))
  expect_error(dag("<-"))
  expect_error(dag("<->"))
  expect_error(dag("X ->"))
  expect_error(dag("<- X"))
  expect_error(dag("<- <- X"))
  expect_error(dag("X -> ->"))
  expect_error(dag("X -> -> Y"))
  expect_error(dag("X -> {}"))
  expect_error(dag("{} -> X"))
  expect_error(dag("{Y -> -> X} -> X"))
  expect_error(dag("<-> X"))
  expect_error(dag("X <->"))
  expect_error(dag("X -> {"))
  expect_error(dag("X -> }"))
  expect_error(dag("{"))
  expect_error(dag("}"))
})

test_that("allowed structures", {
  expect_error(dag("{X -> Y}"), NA)
  expect_error(dag("{X <-> Y} -> {Z -> W}"), NA)
  expect_error(dag("X -> {Y Z} <- W <-> G"), NA)
  expect_error(dag("{X Z} -> {Y W}"), NA)
  expect_error(dag("X -> Z -> Y; X <-> Y"), NA)
  expect_error(dag("{X, Y, Z} -> W"), NA)
  expect_error(dag("Z -> W\nX -> Y"), NA)
  expect_error(dag("{X -> Z} -> {Y <-> W}"), NA)
  expect_error(dag("{X -> {Z -> {Y <-> W}}}"), NA)
})

test_that("latent_projection", {
  expect_identical(dag("X -> Z -> Y", u = "Z"), dag("X -> Y"))
  expect_identical(dag("X <- Z -> Y", u = "Z"), dag("X <-> Y"))
  expect_identical(dag("Z <-> X -> Z -> Y", u = "Z"), dag("X <-> Y; X -> Y"))
  expect_identical(
    dag(" X -> Z -> Y <- W -> X", u = c("Z", "W")),
    dag("X <-> Y; X -> Y")
  )
  expect_identical(
    dag("X -> {Z <-> {A B} -> H} -> Y", u = c("Z", "A", "B", "H")),
    dag("X -> Y")
  )
})

g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
gamma1 <- conj(v4, v1, v2, v3)
p1 <- cfid:::pwg(g1, gamma1)

test_that("generated worlds", {
  expect_identical(sum(p1$latent), 4L)
  expect_identical(p1$n_obs, 15L)
  expect_identical(p1$n_unobs, 4L)
  expect_identical(p1$n_worlds, 3L)
})

# Graph operations --------------------------------------------------------

A <- matrix(
  c(
    0, 1, 0, 1, 0,
    0, 0, 1, 0, 0,
    0, 0, 0, 0, 1,
    0, 0, 1, 0, 0,
    0, 0, 0, 0, 0
  ),
  5,
  5,
  byrow = TRUE
)

test_that("vertex ordering", {
  expect_identical(topological_order(A), c(1L, 2L, 4L, 3L, 5L))
})

test_that("children", {
  expect_setequal(children(1, A), c(2, 4))
  expect_setequal(children(c(2, 4), A), 3)
})

test_that("parents", {
  expect_setequal(parents(c(2, 4), A), 1)
  expect_setequal(parents(5, A), 3)
})

test_that("d-separation", {
  expect_true(dsep(A, 1, 5, c(2, 4)))
  expect_true(dsep(A, 1, 5, 3))
  expect_true(dsep(A, 2, 4, 1))
  expect_false(dsep(A, 1, 5))
  expect_false(dsep(A, 2, 4, 5))
  expect_false(dsep(A, 2, 4))
})

test_that("print", {
  expect_error(
    print.dag(1L),
    "Argument `x` must be a `dag` object"
  )
  expect_output(
    print(dag("X -> Z -> Y")),
    "X -> Z; Z -> Y"
  )
  expect_output(
    print(dag("X -> {Z, W} -> Y")),
    "X -> \\{Z, W\\}; Z -> Y; W -> Y"
  )
  expect_output(
    print(dag("X -> Z; X -> Y; Z -> Y; Z -> W; W -> Y")),
    "X -> \\{Z, Y\\}; Z -> \\{Y, W\\}; W -> Y"
  )
})

# Imported Graphs ---------------------------------------------------------

test_that("dagitty dag supported", {
  dgty <- "dag {\nx\ny\nx -> y\n}\n"
  class(dgty) <- "dagitty"
  expect_identical(import_graph(dgty), dag("x -> y"))
})

test_that("dagitty mag not supported", {
  dgty <- "mag {\nx\ny\nx -- y\n}\n"
  class(dgty) <- "dagitty"
  expect_error(import_graph(dgty))
})

test_that("causaleffect syntax with igraph supported", {
  ig <- igraph::graph_from_literal(X -+ Z -+ Y, Y -+ X, X -+ Y)
  ig <- igraph::set_edge_attr(ig, "description", c(2, 4), "U")
  expect_identical(import_graph(ig), dag("X -> Z -> Y X <-> Y"))
})

test_that("generic igraph not supported", {
  ig <- igraph::graph_from_literal(X -- Y)
  expect_error(import_graph(ig), "Argument `x` must be a directed graph\\.")
})

test_that("fail when no igraph", {
  ig <- igraph::graph_from_literal(X -+ Y)
  mockery::stub(import_graph, "requireNamespace", FALSE)
  expect_error(import_graph(ig), "Attempting to use `igraph` input")
})

test_that("dosearch syntax", {
  g <- "X -> Y\n X -> Z"
  expect_identical(import_graph(g), dag(g))
})

test_that("unsupported format", {
  g <- data.frame()
  expect_error(import_graph(g))
})

# Exported Graphs ---------------------------------------------------------

test_that("dags only", {
  g <- data.frame()
  expect_error(export_graph(g))
})

test_that("dagitty sanity", {
  # dagitty will always enforce alphabetical order it seems
  g <- dag("X <-> Y X -> Z -> Y")
  e <- export_graph(g, "dagitty")
  expect_identical(import_graph(e), g)
})

test_that("fail when no dagitty", {
  g <- dag("X -> Z -> Y <-> X")
  mockery::stub(export_graph, "requireNamespace", FALSE)
  expect_error(export_graph(g, "dagitty"))
})

test_that("causaleffect sanity", {
  g <- dag("X -> Z -> Y X <-> Y ")
  e <- export_graph(g, "causaleffect")
  expect_identical(import_graph(e), g)
})

test_that("fail when no igraph", {
  g <- dag("X -> Z -> Y <-> X")
  mockery::stub(export_graph, "requireNamespace", FALSE)
  expect_error(export_graph(g, "causaleffect"))
})

test_that("dosearch sanity", {
  g <- dag("X -> Z -> Y <-> X")
  e <- export_graph(g, "dosearch")
  expect_identical(import_graph(e), g)
})

test_that("dosearch explicit latent variable", {
  g <- dag("X <-> Y")
  expect_identical(
    export_graph(g, "dosearch", use_bidirected = FALSE),
    "U[X,Y] -> X\nU[X,Y] -> Y"
  )
})
