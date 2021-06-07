# Construct a vertex subgraph
#
# @param x An integer vector giving the vertex indices.
# @param g A `dag` object
subgraph <- function(x, g) {
    h <- g[x, x, drop = FALSE]
    ord <- attr(g, "order")
    attr(h, "labels") <- attr(g, "labels")[x]
    attr(h, "latent") <- attr(g, "latent")[x]
    if (is.logical(x)) {
        x <- which(x)
    }
    attr(h, "order") <- rank(ord[ord %in% x])
    class(h) <- "dag"
    h
}

# Check for cycles in a directed graph.
#
# @param A An adjacency matrix or a `dag` object
detect_cycles <- function(A) {
    X <- A
    n <- ncol(X)
    if (any(diag(X) > 0L)) {
        return(TRUE)
    }
    for (i in 1:(n - 1)) {
        X <- X %*% A
        if (any(diag(X) > 0L)) {
            return(TRUE)
        }
    }
    return(FALSE)
}

# Compute the connected components of an undirected graph.
#
# @param A A symmetric adjacency matrix with a diagonal of 1's.
# @return A list where each element corresponds to a connected component.
components <- function(A) {
    X <- A
    n <- ncol(X)
    if (n > 2L) {
        for (i in 1:(n - 2)) {
            X <- X %*% A
        }
    }
    skip <- logical(n)
    comp <- list()
    j <- 0L
    for (i in 1:n) {
        if (skip[i]) next
        j <- j + 1L
        reach <- which(X[i,] > 0)
        skip[reach] <- TRUE
        comp[[j]] <- reach
    }
    comp
}

# Compute the confounded components of DAG
#
# @param g A `dag` object
# @return A list where each element gives the labels of vertices belonging to that component
c_components <- function(g) {
    fixed <- sapply(attr(g, "labels"), function(x) {
        if (is.CounterfactualVariable(x)) {
            length(x$obs)
        } else {
            0L
        }})
    g <- subgraph(!fixed, g)
    latent <- attr(g, "latent")
    if (any(latent)) {
        lat <- which(latent)
        n_v <- ncol(g) - sum(latent)
        B <- matrix(0L, n_v, n_v)
        for (l in lat) {
            ch_l <- which(g[l,] > 0L)
            ix <- as.matrix(expand.grid(ch_l, ch_l))
            B[ix] <- 1L
        }
        diag(B) <- 1L
    } else {
        B <- diag(ncol(g))
    }
    comp_ix <- components(B)
    lab <- attr(g, "labels")
    lapply(comp_ix, function(x) lab[x])
}

# Check whether a DAG is connected based on its adjacency matrix
#
# @param A An adjacency matrix or a `dag` object
is_connected <- function(A) {
    A_sym <- A + t(A)
    n <- ncol(A_sym)
    diag(A_sym) <- 1L
    length(components(A_sym)) == 1L
}

# Computes a topological ordering for the vertices of a DAG.
#
# @param A An adjancecy matrix or a `dag` object
# @param latent An optional logical vector with
#     `TRUE` indicating latent variables
topological_order <- function(A, latent) {
    n <- ncol(A)
    if (missing(latent)) {
        latent <- logical(n)
    }
    lat <- which(latent)
    ord <- integer(n)
    v <- 1L:n
    j <- 1L
    if (n_unobs <- length(lat)) {
        ord[1:n_unobs] <- lat
        j <- n_unobs + 1L
        A <- A[-lat, -lat, drop = FALSE]
        v <- v[-lat]
    }
    while (j <= n) {
        roots <- which(!colSums(A))
        n_r <- length(roots)
        ord[j:(j + n_r - 1)] <- v[roots]
        v <- v[-roots]
        A <- A[-roots, -roots, drop = FALSE]
        j <- j + n_r
    }
    ord
}

# Ancestors of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `dag` object.
ancestors <- function(x, A) {
    an <- parents(x, A)
    if (length(an)) {
        an <- union(an, ancestors(an, A))
    }
    an
}

# Children of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `dag` object.
children <- function(x, A) {
    if (length(x) == 1L) which(A[x,] > 0L)
    else which(A[x,] > 0L, arr.ind = TRUE)[,2]
}

# Descendants of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `dag` object.
descendants <- function(x, A) {
    de <- children(x, A)
    if (length(de)) {
        de <- union(de, descendants(de, A))
    }
    de
}

# Parents of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `dag` object.
parents <- function(x, A) {
    if (length(x) == 1L) which(A[,x] > 0L)
    else which(A[,x] > 0L, arr.ind = TRUE)[,1]
}

# Test for d-separation, adapted from the causaleffect package
# Implements relevant path separation (rp-separation) for testing d-separation. For details, see:
#
# Relevant Path Separation: A Faster Method for Testing Independencies in Bayesian Networks
# Cory J. Butz, Andre E. dos Santos, Jhonatan S. Oliveira;
# Proceedings of the Eighth International Conference on Probabilistic Graphical Models,
# PMLR 52:74-85, 2016.
#
# Note that the roles of Y and Z have been reversed from the paper, meaning that
# we are testing whether X is separated from Y given Z in G.
#
# @param g A `dag` object.
# @param x An integer vector of vertex indices.
# @param y An integer vector of vertex indices.
# @param z An integer vector of vertex indices (optional).
# @return `TRUE` if X _||_ Y | Z in g, else `FALSE`.
dsep <- function(g, x, y, z = integer(0)) {
    if (length(z)) {
        an_z <- union(ancestors(z, g), z)
    } else {
        an_z <- integer(0)
    }
    xyz <- union(union(x, y), z)
    an_xyz <- union(ancestors(xyz, g), xyz)
    stack_top <- length(x)
    stack_size <- max(stack_top, 64)
    stack <- rep(FALSE, stack_size)
    stack[1:stack_top] <- TRUE
    names(stack)[1:stack_top] <- x
    visited_top <- 0
    visited_size <- 64
    visited <- rep(FALSE, visited_size)
    names(visited) <- rep(NA, visited_size)
    is_visited <- FALSE
    while (stack_top > 0) {
        is_visited <- FALSE
        el <- stack[stack_top]
        el_name <- names(el)
        el_var <- as.integer(el_name)
        stack_top <- stack_top - 1
        if (visited_top > 0) {
            for (i in 1:visited_top) {
                if (el == visited[i] && identical(el_name, names(visited[i]))) {
                    is_visited <- TRUE
                    break
                }
            }
        }
        if (!is_visited) {
            if (el_var %in% y) {
                return(FALSE)
            }
            visited_top <- visited_top + 1
            if (visited_top > visited_size) {
                visited_old <- visited
                visited_size_old <- visited_size
                visited_size <- visited_size * 2
                visited <- rep(FALSE, visited_size)
                visited[1:visited_size_old] <- visited_old
                names(visited[1:visited_size_old]) <- names(visited_old)
            }
            visited[visited_top] <- el
            names(visited)[visited_top] <- el_name
            if (el && !(el_var %in% z)) {
                visitable_parents <- intersect(parents(el_var, g), an_xyz)
                visitable_children <- intersect(children(el_var, g), an_xyz)
                n_vis_pa <- length(visitable_parents)
                n_vis_ch <- length(visitable_children)
                if (n_vis_pa + n_vis_ch > 0) {
                    while (n_vis_pa + n_vis_ch + stack_top > stack_size) {
                        stack_old <- stack
                        stack_size_old <- stack_size
                        stack_size <- stack_size * 2
                        stack <- rep(FALSE, stack_size)
                        stack[1:stack_size_old] <- stack_old
                        names(stack[1:stack_size_old]) <- names(stack_old)
                    }
                    stack_add <- stack_top + n_vis_pa + n_vis_ch
                    stack[(stack_top + 1):(stack_add)] <- c(rep(TRUE, n_vis_pa), rep(FALSE, n_vis_ch))
                    names(stack)[(stack_top + 1):(stack_add)] <- c(visitable_parents, visitable_children)
                    stack_top <- stack_add
                }
            } else if (!el) {
                if (!(el_var %in% z)) {
                    visitable_children <- intersect(children(el_var, g), an_xyz)
                    n_vis_ch <- length(visitable_children)
                    if (n_vis_ch > 0) {
                        while (n_vis_ch + stack_top > stack_size) {
                            stack_old <- stack
                            stack_size_old <- stack_size
                            stack_size <- stack_size * 2
                            stack <- rep(FALSE, stack_size)
                            stack[1:stack_size_old] <- stack_old
                            names(stack[1:stack_size_old]) <- names(stack_old)
                        }
                        stack_add <- stack_top + n_vis_ch
                        stack[(stack_top + 1):(stack_add)] <- rep(FALSE, n_vis_ch)
                        names(stack)[(stack_top + 1):(stack_add)] <- visitable_children
                        stack_top <- stack_add
                    }
                }
                if (el_var %in% an_z) {
                    visitable_parents <- intersect(parents(el_var, g), an_xyz)
                    n_vis_pa <- length(visitable_parents)
                    if (n_vis_pa > 0) {
                        while (n_vis_pa + stack_top > stack_size) {
                            stack_old <- stack
                            stack_size_old <- stack_size
                            stack_size <- stack_size * 2
                            stack <- rep(FALSE, stack_size)
                            stack[1:stack_size_old] <- stack_old
                            names(stack[1:stack_size_old] <- stack_old)
                        }
                        stack_add <- stack_top + n_vis_pa
                        stack[(stack_top + 1):(stack_add)] <- rep(TRUE, n_vis_pa)
                        names(stack)[(stack_top + 1):(stack_add)] <- visitable_parents
                        stack_top <- stack_add
                    }
                }
            }
        }
    }
    return(TRUE)
}
