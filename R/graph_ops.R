# Construct a vertex subgraph
#
# @param x An integer vector giving the vertex indices.
# @param g A `DAG` object
subgraph <- function(x, g) {
    h <- g[x, x, drop = FALSE]
    ord <- attr(g, "order")
    attr(h, "labels") <- attr(g, "labels")[x]
    attr(h, "latent") <- attr(g, "latent")[x]
    if (is.logical(x)) {
        x <- which(x)
    }
    attr(h, "order") <- rank(ord[ord %in% x])
    class(h) <- "DAG"
    h
}

# Check for cycles in a directed graph.
#
# @param A An adjacency matrix or a `DAG` object
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
# @param g A `DAG` object
# @return A list where each element gives the labels of vertices belonging to that component
c_components <- function(g) {
    fixed <- sapply(attr(g, "labels"), function(x) {
        length(x$obs)
    })
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
# is_connected <- function(A) {
#     A_sym <- A + t(A)
#     n <- ncol(A_sym)
#     diag(A_sym) <- 1L
#     length(components(A_sym)) == 1L
# }

# Computes a topological ordering for the vertices of a DAG.
#
# @param A An adjancecy matrix or a `DAG` object
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
# @param A An adjancecy matrix or a `DAG` object.
ancestors <- function(x, A) {
    B <- t(A)
    diag(B) <- 1L
    X <- B
    n <- ncol(X)
    for (i in 1:(n - 1)) {
        X <- X %*% B
    }
    setdiff(children(x, X), x)
}

# Children of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `DAG` object.
children <- function(x, A) {
    if (length(x) == 1L) which(A[x,] > 0L)
    else unique(which(A[x,] > 0L, arr.ind = TRUE)[,2])
}

# Descendants of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `DAG` object.
descendants <- function(x, A) {
    diag(A) <- 1L
    X <- A
    n <- ncol(X)
    for (i in 1:(n - 1)) {
        X <- X %*% A
    }
    setdiff(children(x, X), x)
}

# Parents of a vertex set
#
# @param x An integer vector giving the vertex indices.
# @param A An adjancecy matrix or a `DAG` object.
parents <- function(x, A) {
    if (length(x) == 1L) which(A[,x] > 0L)
    else unique(which(A[,x] > 0L, arr.ind = TRUE)[,1])
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
# @param g A `DAG` object.
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
    n <- ncol(g)
    xyz <- union(union(x, y), z)
    an_xyz <- union(ancestors(xyz, g), xyz)
    # indices from 1:n = 1st direction ("up"), map to TRUE
    # the rest = 2nd direction ("down"), map to FALSE
    L <- logical(2L * n)
    V <- logical(2L * n)
    L[x] <- TRUE
    while (any(L)) {
        el <- which(L)[1]
        L[el] <- FALSE
        d <- el <= n
        v <- el - (n * !d)
        if (!V[el]) {
            if (v %in% y) {
                return(FALSE)
            }
            V[el] <- TRUE
            if (d && !(v %in% z)) {
                vis_pa <- intersect(parents(v, g), an_xyz)
                vis_ch <- intersect(children(v, g), an_xyz)
                if (length(vis_pa)) {
                    L[vis_pa] <- TRUE
                }
                if (length(vis_ch)) {
                    L[vis_ch + n] <- TRUE
                }
            } else if (!d) {
                if (!(v %in% z)) {
                    vis_ch <- intersect(children(v, g), an_xyz)
                    if (length(vis_ch)) {
                        L[vis_ch + n] <- TRUE
                    }
                }
                if (v %in% an_z) {
                    vis_pa <- intersect(parents(v, g), an_xyz)
                    if (length(vis_pa)) {
                        L[vis_pa] <- TRUE
                    }
                }
            }
        }
    }
    TRUE
}
