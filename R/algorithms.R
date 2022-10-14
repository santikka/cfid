#' The ID* Algorithm
#'
#' @param g A `dag` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @return A `list` with two components: `id` and `formula` giving
#' the identifiability status as a `logical` value and the identifying
#' functional as a `functional` or a `probability` object, respectively.
#' @noRd
id_star <- function(g, gamma) {
  # Line 2
  if (is_inconsistent(gamma)) {
    return(list(id = TRUE, formula = probability(val = 0L)))
  }
  # Line 3
  gamma <- remove_tautologies(gamma)
  # Line 1
  if (length(gamma) == 0L) {
    return(list(id = TRUE, formula = probability(val = 1L)))
  }
  # Line 4
  tmp <- make_cg(g, gamma)
  # Line 5
  if (!tmp$consistent) {
    return(list(id = TRUE, formula = probability(val = 0L)))
  }
  g_prime <- tmp$graph
  lab_prime <- attr(g_prime, "labels")
  v_g <- unique(
    vars(lab_prime[!attr(g_prime, "latent") & !assigned(lab_prime)])
  )
  gamma_prime <- tmp$conjunction
  gamma_var <- vars(gamma)
  gamma_obs <- obs(gamma)
  gamma_obs_var <- vars(gamma_obs)
  merged <- tmp$merged
  comp <- c_components(g_prime)
  n_comp <- length(comp)
  if (n_comp > 1L) {
    # Line 6
    c_factors <- vector(mode = "list", length = n_comp)
    prob_zero <- FALSE
    for (i in seq_len(n_comp)) {
      s_var <- vars(comp[[i]])
      s_sub <- setdiff(v_g, s_var)
      for (j in seq_along(comp[[i]])) {
        gamma_val <- val(comp[[i]][[j]], gamma_prime)
        comp[[i]][[j]]$obs <- ifelse_(is.null(gamma_val), 0L, gamma_val)
        sub_var <- names(comp[[i]][[j]]$sub)
        s_sub_j <- setdiff(s_sub, sub_var)
        s_len <- length(s_sub_j)
        if (s_len > 0) {
          sub_new <- set_names(integer(s_len), s_sub_j)
          obs_ix <- which(gamma_obs_var %in% s_sub_j)
          if (length(obs_ix) > 0) {
            s_val <- unlist(evs(gamma_obs)[obs_ix])
            s_ix <- which(s_sub_j %in% gamma_obs_var)
            sub_new[s_ix] <- s_val
          }
          comp[[i]][[j]]$sub <- c(comp[[i]][[j]]$sub, sub_new)
        }
      }
      #s_conj <- do.call(counterfactual_conjunction, comp[[i]])
      s_conj <- try(
        do.call(counterfactual_conjunction, comp[[i]]), silent = TRUE
      )
      if (inherits(s_conj, "try-error")) {
        return(list(id = TRUE, formula = probability(val = 0L)))
      }
      c_factors[[i]] <- id_star(g, s_conj)
      if (!c_factors[[i]]$id) {
        return(list(id = FALSE, formula = NULL))
      }
      if (is.probability(c_factors[[i]]$formula) &&
          length(c_factors[[i]]$formula$val) > 0L &&
          c_factors[[i]]$formula$val == 0L) {
        return(list(id = TRUE, formula = probability(val = 0L)))
      }
    }
    sumset <- setdiff(v_g, gamma_var)
    form_terms <- lapply(c_factors, "[[", "formula")
    if (length(sumset) > 0L) {
      form_out <- functional(
        sumset = lapply(sumset, function(x) cf(var = x, obs = 0L)),
        terms = form_terms
      )
    } else {
      form_out <- functional(terms = form_terms)
    }
    return(list(id = TRUE, formula = form_out))

  }
  # Line 7
  gamma_sub <- subs(gamma)
  gamma_cfvar <- cfvars(gamma_prime)
  gamma_sub_var <- lapply(gamma_sub, names)
  for (i in seq_along(gamma_prime)) {
    if (length(gamma_sub_var[[i]]) > 0L) {
      g_ix <- which(lab_prime %in% gamma_cfvar[i])
      an <- ancestors(g_ix, g_prime)
      an_var <- vars(lab_prime[an])
      an_sub <- which(gamma_sub_var[[i]] %in% an_var)
      gamma_prime[[i]]$sub <- gamma_prime[[i]]$sub[an_sub]
    }
  }
  s_sub <- unlist(unique(subs(gamma_prime)))
  s_sub <- split(s_sub, names(s_sub))
  if (any(lengths(s_sub) > 1L)) {
    return(list(id = FALSE, formula = NULL))
  }
  s_val <- unlist(evs(gamma_prime))
  s_val <- split(s_val, names(s_val))
  s_val_names <- names(s_val)
  for (i in seq_along(s_val)) {
    uniq_val <- unique(s_val[[i]])
    n_val <- length(uniq_val)
    if (n_val > 1L) {
      val_pairs <- which(
        lower.tri(matrix(0L, n_val, n_val)),
        arr.ind = TRUE
      )
      sub_i <- s_sub[[s_val_names[[i]]]]
      val_i <- s_val[[s_val_names[[i]]]]
      for (j in seq_len(nrow(val_pairs))) {
        if (val_i[val_pairs[, 1L]] %in% sub_i ||
            val_i[val_pairs[, 2L]] %in% sub_i) {
          return(list(id = FALSE, formula = NULL))
        }
      }
    }
  }
  lab <- attr(g, "labels")
  obs <- vals(gamma_prime)
  obs_var <- vars(obs)
  obs_ix <- which(lab %in% obs_var)
  obs_an <- ancestors(obs_ix, g)
  s_sub_an <- intersect(names(s_sub), lab[obs_an])
  if (length(s_sub_an) > 0L) {
    do <- lapply(s_sub_an, function(i) cf(var = i, obs = s_sub[i]))
  } else {
    do <- integer(0L)
  }
  list(id = TRUE, formula = probability(var = obs, do = do))
}

#' The IDC* Algorithm
#'
#' @param g A `dag` object.
#' @param gamma A `counterfactual_conjunction` object.
#' @param delta A `counterfactual_conjunction` object.
#' @return A `list` with two components: `id` and `formula` giving
#' the identifiability status as a `logical` value and the identifying
#' functional as a `functional` or a `probability` object, respectively.
#' If the probability is undefined, a `list` with the components `id`
#' and `undefined` are returned, with values `FALSE` and `TRUE`, respectively.
#' @noRd
idc_star <- function(g, gamma, delta) {
  if (length(delta) == 0L) {
    return(id_star(g, gamma))
  }
  delta_out <- id_star(g, delta)
  if (is.probability(delta_out$formula) &&
      length(delta_out$formula$val) > 0L &&
      delta_out$formula$val == 0L) {
    return(list(id = FALSE, undefined = TRUE))
  }
  gamma_delta <- try(gamma + delta, silent = TRUE)
  if (inherits(gamma_delta, "try-error")) {
    # Inconsistent directly after merge
    return(list(id = TRUE, formula = probability(val = 0L)))
  }
  tmp <- make_cg(g, gamma_delta)
  if (!tmp$consistent) {
    return(list(id = TRUE, formula = probability(val = 0L)))
  }
  gamma_orig_ix <- which(gamma_delta %in% gamma)
  delta_orig_ix <- which(gamma_delta %in% delta)
  g_prime <- tmp$graph
  lab <- attr(g, "labels")
  lab_prime <- attr(g_prime, "labels")
  gamma_ix <- which(lab %in% vars(gamma))
  gamma_prime <- tmp$conjunction[gamma_orig_ix]
  gamma_prime_ix <- which(lab_prime %in% cfvars(gamma_prime))
  delta_prime <- tmp$conjunction[delta_orig_ix]
  delta_var <- vars(delta_prime)
  delta_cfvar <- cfvars(delta_prime)
  fixed_ix <- fixed(g_prime)
  delta_nonfixed <- which(delta_cfvar %in% lab_prime)
  for (i in delta_nonfixed) {
    d_prime_ix <- which(lab_prime %in% delta_cfvar[i])
    g_temp <- g_prime
    g_temp[d_prime_ix, ] <- 0L
    if (dsep(g_temp, d_prime_ix, gamma_prime_ix, fixed_ix)) {
      d_ix <- which(lab %in% delta_var[i])
      de <- intersect(descendants(d_ix, g), gamma_ix)
      new_sub <- set_names(delta_prime[[i]]$obs, delta_prime[[i]]$var)
      de_gamma <- which(vars(gamma) %in% lab)
      for (j in de_gamma) {
        if (!delta_prime[[i]]$var %in% names(gamma_prime[[j]]$sub)) {
          gamma_prime[[j]]$sub <- c(gamma_prime[[j]]$sub, new_sub)
        }
      }
      return(idc_star(g, gamma_prime, delta_prime[-i]))
    }
  }
  gamma_delta <- gamma_prime + delta_prime
  out <- id_star(g, gamma_delta)
  if (out$id) {
    if ((is.probability(out$formula) && length(out$formula$val) == 0L) ||
        is.functional(out$formula)) {
      if (identical(gamma_prime, gamma_delta)) {
        out$formula <- probability(val = 1L)
      } else if (is.probability(out$formula)) {
        gamma_vars <- which(vars(out$formula$var) %in% vars(gamma_prime))
        delta_vars <- which(vars(out$formula$var) %in% vars(delta_prime))
        out$formula <- probability(
          var = out$formula$var[gamma_vars],
          do = out$formula$do,
          cond = out$formula$var[delta_vars]
        )
      } else {
        num <- out$formula
        den <- id_star(g, delta_prime)$formula
        out$formula <- functional(numerator = num, denominator = den)
      }
    }
  }
  out
}

#' The ID Algorithm
#'
#' @param y_var A `character` vector of response variables.
#' @param x_var A `character` vector of intervention variables.
#' @param p A `probability` object
#' @param g A `dag` object.
#' @return A `list` with two components: `id` and `formula` giving
#' the identifiability status as a `logical` value and the identifying
#' functional as a `functional` or a `probability` object, respectively.
#' @noRd
id <- function(y_var, x_var, p, g) {
  lat <- attr(g, "latent")
  pi_obs <- setdiff(attr(g, "order"), which(lat))
  vu_var <- attr(g, "labels")
  vu <- seq_len(ncol(g))
  v <- vu[!lat]
  ord <- order(match(v, pi_obs))
  v <- v[ord]
  v_var <- vu_var[!lat]
  v_var <- v_var[ord]
  y <- which(vu_var %in% y_var)
  x <- which(vu_var %in% x_var)
  # Line 1
  if (length(x) == 0) {
    if (is.probability(p)) {
      p <- probability(var = cflist(y_var))
    } else {
      p$sumset <- union(p$sumset, cflist(vu_var[setdiff(v, y)]))
    }
    return(list(id = TRUE, formula = p))
  }
  # Line 2
  an <- sort(union(ancestors(y, g), y))
  if (length(setdiff(vu, an)) > 0) {
    an_var <- vu_var[intersect(v, an)]
    if (is.probability(p)) {
      p <- probability(var = cflist(an_var))
    } else {
      p$sumset <- union(p$sumset, cflist(setdiff(v_var, an_var)))
    }
    return(id(y_var, intersect(an_var, x_var), p, subgraph(an, g)))
  }
  # Line 3
  g_xbar <- g
  g_xbar[, x] <- 0L
  vmx <- setdiff(v, x)
  w <- setdiff(vmx, union(ancestors(y, g_xbar), y))
  if (length(w) > 0L) {
    return(id(y_var, vu_var[union(x, w)], p, g))
  }
  # Line 4
  comp_gmx <- c_components(subgraph(setdiff(vu, x), g))
  n_s <- length(comp_gmx)
  if (n_s > 1L) {
    c_factors <- vector(mode = "list", length = n_s)
    for (i in seq_len(n_s)) {
      s_var <- comp_gmx[[i]]
      c_factors[[i]] <- id(s_var, setdiff(v_var, s_var), p, g)
      if (!c_factors[[i]]$id) {
        return(list(id = FALSE, formula = NULL))
      }
    }
    form_terms <- lapply(c_factors, "[[", "formula")
    return(
      list(
        id = TRUE,
        formula = functional(
          sumset = cflist(vu_var[setdiff(v, union(x, y))]),
          terms = form_terms
        )
      )
    )
  }
  # Line 5
  comp_g <- c_components(g)
  if (setequal(comp_g[[1L]], v_var)) {
    return(list(id = FALSE, formula = NULL))
  }
  # Line 6
  s_var <- comp_gmx[[1L]]
  pos <- Position(function(i) identical(s_var, i), comp_g, nomatch = 0L)
  if (pos > 0L) {
    return(
      list(
        id = TRUE,
        formula = functional(
          sumset = cflist(setdiff(s_var, y_var)),
          terms = factorize_probability(s_var, v_var, p)
        )
      )
    )
  }
  # Line 7
  s_prime_var <- Find(function(i) all(s_var %in% i), comp_g)
  p_new <- functional(
    terms = factorize_probability(s_prime_var, v_var, p)
  )
  s_prime <- which(vu_var %in% s_prime_var)
  s_prime_lat <- intersect(parents(s_prime, g), which(lat))
  s_vu <- union(s_prime, s_prime_lat)
  id(y_var, intersect(s_prime_var, x_var), p_new, subgraph(s_vu, g))
}

#' The IDC Algorithm
#'
#' @param y_var A `character` vector of response variables.
#' @param x_var A `character` vector of intervention variables.
#' @param z_var A `character` vector of conditioning variables.
#' @param p A `probability` object
#' @param g A `dag` object.
#' @return A `list` with two components: `id` and `formula` giving
#' the identifiability status as a `logical` value and the identifying
#' functional as a `functional` or a `probability` object, respectively.
#' @noRd
idc <- function(y_var, x_var, z_var, p, g) {
  lat <- attr(g, "latent")
  pi_obs <- setdiff(attr(g, "order"), which(lat))
  vu_var <- attr(g, "labels")
  vu <- seq_len(ncol(g))
  v <- vu[!lat]
  ord <- order(match(v, pi_obs))
  v <- v[ord]
  v_var <- vu_var[!lat]
  v_var <- v_var[ord]
  y <- which(vu_var %in% y_var)
  x <- which(vu_var %in% x_var)
  z <- which(vu_var %in% z_var)
  # Line 1
  for (zi in z) {
    g_xz <- g
    g_xz[, x] <- 0L
    g_xz[zi, ] <- 0L
    zmzi <- setdiff(z, zi)
    if (dsep(g_xz, y, zi, union(x, zmzi))) {
      return(idc(y_var, union(x_var, vu_var[zi]), vu_var[zmzi], p, g))
    }
  }
  # Line 2
  if (length(z_var) == 0) {
    # Simplify cases where denominator would be equal to 1.
    return(id(y_var, x_var, p, g))
  }
  out <- id(union(y_var, z_var), x_var, p, g)
  if (!out$id) {
    return(list(id = FALSE, formula = NULL))
  }
  d <- out$formula
  d$sumset <- union(d$sumset, cflist(y_var))
  list(
    id = TRUE,
    formula = functional(
      numerator = out$formula,
      denominator = d
    )
  )
}

#' Compute the C-component Factorization of a Probability Distribution
#'
#' @param s_var A `character` vector of the variables in the C-component.
#' @param v_var A `character` vector of observed variables in the graph.
#' @param p A `probability` object.
#' @return The C-factors a `list` of `probability` or `functional` objects.
#' @noRd
factorize_probability <- function(s_var, v_var, p) {
  n_s <- length(s_var)
  p_terms <- vector(mode = "list", length = n_s)
  s_var <- s_var[order(match(s_var, v_var))]
  if (is.probability(p)) {
    for (i in seq_len(n_s)) {
      j <- which(v_var == s_var[i])
      cond_vars <- cflist(v_var[seq_len(j - 1L)])
      p_terms[[n_s + 1 - i]] <- probability(
        var = list(cf(s_var[i])),
        cond = cond_vars
      )
    }
  } else {
    for (i in seq_len(n_s)) {
      j <- which(v_var == s_var[i])
      n <- p
      d <- p
      n$sumset <- union(
        p$sumset,
        cflist(setdiff(v_var, v_var[seq_len(j)]))
      )
      d$sumset <- union(
        p$sumset,
        cflist(setdiff(v_var, v_var[seq_len(j - 1L)]))
      )
      if (length(d$sumset) == length(d$terms) && is.null(d$numerator)) {
        p_terms[[n_s + 1 - i]] <- n
      } else {
        p_terms[[n_s + 1 - i]] <- functional(numerator = n, denominator = d)
      }
    }
  }
  p_terms
}
