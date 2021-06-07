# The ID* algorithm
#
# @param g An object of class `dag`.
# @param gamma An object of class `CounterfactualConjunction`
id_star <- function(g, gamma) {
    n_cf <- length(gamma)
    if (!n_cf) {
        return(list(id = TRUE, prob = Probability(val = 1L)))
    }
    taut <- logical(n_cf)
    for (i in 1:n_cf) {
        if (is_inconsistent(gamma[[i]])) {
            return(list(id = TRUE, prob = Probability(val = 0L)))
        }
        if (is_tautology(gamma[[i]])) {
            taut[i] <- TRUE
        }
    }
    if (any(taut)) {
        return(id_star(g, gamma[!taut]))
    }
    tmp <- make_cg(g, gamma)
    if (!tmp$consistent) {
        return(list(id = TRUE, prob = Probability(val = 0L)))
    }
    g_prime <- tmp$graph
    lab_prime <- attr(g_prime, "labels")
    v_g <- unique(vars(lab_prime[!attr(g_prime, "latent") & !fixed(lab_prime)]))
    gamma_prime <- tmp$conjunction
    gamma_vars <- vars(gamma)
    merged <- tmp$merged
    comp <- c_components(g_prime)
    n_comp <- length(comp)
    if (n_comp > 1L) {
        c_factors <- vector(mode = "list", length = n_comp)
        prob_zero <- FALSE
        for (i in 1:n_comp) {
            s_vars <- vars(comp[[i]])
            s_intv <- setdiff(v_g, s_vars)
            for (j in seq_along(comp[[i]])) {
                gamma_val <- val(comp[[i]][[j]], gamma_prime)
                if (!is.null(gamma_val)) {
                    comp[[i]][[j]]$obs <- gamma_val
                } else {
                    comp[[i]][[j]]$obs <- 0
                }
                if (length(intv_vars <- names(comp[[i]][[j]]$int))) {
                    s_intv <- setdiff(s_intv, intv_vars)
                }
                comp[[i]][[j]]$int <- c(comp[[i]][[j]]$int,
                                        setNames(integer(length(s_intv)), s_intv))
            }
            s_conj <- do.call(CounterfactualConjunction, comp[[i]])
            c_factors[[i]] <- id_star(g, s_conj)
            if (!c_factors[[i]]$id) {
                return(list(id = FALSE))
            }
            if (length(c_factors[[i]]$prob$val) && c_factors[[i]]$prob$val == 0L) {
                prob_zero <- TRUE
            }
        }
        if (prob_zero) {
            return(list(id = TRUE, prob = Probability(val = 0L)))
        } else {
            sumset <- setdiff(v_g, gamma_vars)
            prob_terms <- Probability(terms = lapply(c_factors, "[[", "prob"))
            if (length(sumset)) {
                prob_out <- Probability(
                    sumset = lapply(sumset, function(x) cf(var = x, obs = 0L)),
                    summand = prob_terms)
            } else {
                prob_out <- prob_terms
            }
            return(list(id = TRUE, prob = prob_out))
        }
    } else {
        gamma_ints <- ints(gamma)
        gamma_cfvars <- cfvars(gamma_prime)
        gamma_intv_vars <- lapply(gamma_ints, names)
        for (i in seq_along(gamma_prime)) {
            if (length(gamma_intv_vars[[i]])) {
                g_ix <- which(lab_prime %in% gamma_cfvars[i])
                an <- ancestors(g_ix, g_prime)
                an_vars <- vars(lab_prime[an])
                an_intv <- which(gamma_intv_vars[[i]] %in% an_vars)
                gamma_prime[[i]]$int <- gamma_prime[[i]]$int[an_intv]
            }
        }
        S_intv <- unlist(unique(ints(gamma_prime)))
        S_intv <- split(S_intv, names(S_intv))
        if (any(sapply(S_intv, length) > 1L)) {
            return(list(id = FALSE))
        }
        S_vals <- unlist(evs(gamma_prime))
        S_vals <- split(S_vals, names(S_vals))
        S_val_names <- names(S_vals)
        for (i in seq_along(S_vals)) {
            uniq_vals <- unique(S_vals[[i]])
            n_vals <- length(uniq_vals)
            if (n_vals > 1) {
                val_pairs <- which(lower.tri(matrix(0L, n_vals, n_vals)),
                                   arr.ind = TRUE)
                intv_i <- S_intv[[S_val_names[[i]]]]
                vals_i <- S_vals[[S_val_names[[i]]]]
                for (j in 1:nrow(val_pairs)) {
                    if (vals_i[val_pairs[,1]] %in% intv_i ||
                            vals_i[val_pairs[,2]] %in% intv_i) {
                        return(list(id = FALSE))
                    }
                }
            }
        }
        lab <- attr(g, "labels")
        obs <- vals(gamma_prime)
        obs_vars <- vars(obs)
        obs_ix <- which(lab %in% obs_vars)
        obs_an <- ancestors(obs_ix, g)
        S_intv_an <- intersect(names(S_intv), lab[obs_an])
        if (length(S_intv_an)) {
            do <- lapply(S_intv_an, function(i) cf(var = i, obs = S_intv[i]))
        } else {
            do <- integer(0)
        }
        return(list(
            id = TRUE,
            prob = Probability(var = obs, do = do)
        ))
    }
}

# The IDC* algorithm
#
# @param g An object of class `dag`.
# @param gamma An object of class `CounterfactualConjunction`
# @param delta An object of class `CounterfactualConjunction`
idc_star <- function(g, gamma, delta) {
    delta_out <- id_star(g, delta)
    if (!delta_out$id) {
        return(delta_out)
    }
    if (length(delta_out$prob$val) && delta_out$prob$val == 0L) {
        return(list(id = FALSE, undefined = TRUE))
    }
    n_gamma <- length(gamma)
    n_delta <- length(delta)
    tmp <- make_cg(g, gamma + delta)
    if (!tmp$consistent) {
        return(list(id = TRUE, prob = Probability(val = 0L)))
    }
    g_prime <- tmp$graph
    lab <- attr(g, "labels")
    lab_prime <- attr(g_prime, "labels")
    gamma_ix <- which(lab %in% vars(gamma))
    gamma_prime <- tmp$conjunction[1:n_gamma]
    gamma_prime_ix <- which(lab_prime %in% cfvars(gamma_prime))
    delta_prime <- tmp$conjunction[(n_gamma + 1):(n_gamma + n_delta)]
    delta_vars <- vars(delta_prime)
    delta_cfvars <- cfvars(delta_prime)
    for (i in seq_along(delta_vars)) {
        d_ix <- which(lab %in% delta_vars[i])
        d_prime_ix <- which(lab_prime %in% delta_cfvars[i])
        g_temp <- g_prime
        g_temp[d_ix,] <- 0L
        if (dsep(g_temp, d_prime_ix, gamma_ix)) {
            de <- intersect(descendants(d_ix, g), gamma_ix)
            d <- delta_prime[[i]]
            for (j in seq_along(de)) {
                gamma_prime[[j]]$int <- c(gamma_prime[[j]]$int,
                                          setNames(d$obs, d$var))
            }
            return(idc_star(g, gamma_prime, delta_prime[-i]))
        }
    }
    out <- id_star(g, gamma_prime + delta_prime)
    if (out$id) {
        if (!length(out$prob$val)) {
            out$prob <- Probability(numerator = out$prob,
                                    denominator = id_star(g, delta_prime)$prob)
        }
    }
    out
}
