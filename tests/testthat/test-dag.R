test_that("no self-loops or cycles", {
    expect_error(dag("X -> X"))
    expect_error(dag("X -> Z -> X"))
    expect_error(dag("X -> Z -> Y -> X"))
})

test_that("extra delimiter invariance", {
    expect_identical(dag("X -> Y -> Z"), dag("X -> Y; Y -> Z"))
    expect_identical(dag("X -> Y -> Z"), dag("X -> Y\n Y -> Z"))
    expect_identical(dag("{X, Y, Z}"), dag("X Y Z"))
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

test_that("no edges within groups", {
    expect_error(dag("{X -> Y}"))
    expect_error(dag("{X <-> Y} -> {Z -> W}"))
})

test_that("missing endpoint", {
    expect_error(dag("X ->"))
    expect_error(dag("<- X"))
    expect_error(dag("<-> X"))
    expect_error(dag("X <->"))
})
