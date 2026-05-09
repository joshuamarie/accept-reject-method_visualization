box::use(
    dplyr[
        tbl = tibble, keep_when = filter, mutate, bind_rows, if_else, slice
    ],
    stats[runif, rnorm, rgamma, dunif, dnorm, dgamma],
    dqrng[dqrunif],
    rlang[new_function, pairlist2, expr]
)

#' Accept-Reject Algorithm
#'
#' @param n Integer. Number of samples to be accepted to collect.
#' @param propose The given is a list of components.
#'
#' @return A `tbl_df` data frame with columns: x, y, status ("accepted" | "rejected")
#'
#' @export
sampler = function(n, propose) {
    accepted = tbl(
        x = numeric(),
        y = numeric(),
        status = character()
    )
    rejected = tbl(
        x = numeric(),
        y = numeric(),
        status = character()
    )

    while (nrow(accepted) < n) {
        remaining_n = n - nrow(accepted)
        new_n = max(remaining_n, ceiling(remaining_n * 1.5))

        x = propose$propose(new_n)
        u = dqrunif(new_n, min = 0, max = 1)
        y = propose$M * u * propose$density(x)

        batch = tbl(x = x, y = y) |>
            mutate(
                status = if_else(y <= propose$pdf(x), "accepted", "rejected")
            )

        accepted = bind_rows(accepted, keep_when(batch, status == "accepted"))
        rejected = bind_rows(rejected, keep_when(batch, status == "rejected"))
    }

    bind_rows(
        slice(accepted, seq_len(n)),
        rejected
    )
}

#' Build proposal metadata for a known distribution
#'
#' Applied with `[rlang::new_function()]` to construct propose/density closures
#' dynamically. This keeps the `switch()` arms declarative. No repeated `function(...)`
#' boilerplate.
#'
#' @param tab Character. One of "Uniform", "Gaussian", "Gamma".
#' @param params A known_dists object from params$known_dists().
#'
#' @return A named list: propose, density, M, pdf
#' @export
known_dist_metadata = function(tab, params, pdf) {
    p = switch(
        tab,
        Uniform = params$uniform,
        Gaussian = params$normal,
        Gamma = params$gamma,
        stop(paste("Unknown proposal tab:", tab))
    )

    switch(
        tab,
        Uniform = list(
            propose = new_function(
                pairlist2(k = ),
                expr(
                    # runif(k, min = {{ p$min }}, max = {{ p$max }})
                    runif(k, min = !!p$min, max = !!p$max)
                )
            ),
            density = new_function(
                pairlist2(x = ),
                expr(
                    # dunif(x, min = {{ p$min }}, max = {{ p$max }})
                    dunif(x, min = !!p$min, max = !!p$max)
                )
            ),
            M = p$M,
            pdf = pdf
        ),
        Gaussian = list(
            propose = new_function(
                pairlist2(k = ),
                expr(
                    # rnorm(k, mean = {{ p$mean }}, sd = {{ p$sd }})
                    rnorm(k, mean = !!p$mean, sd = !!p$sd)
                )
            ),
            density = new_function(
                pairlist2(x = ),
                expr(
                    # dnorm(x, mean = {{ p$mean }}, sd = {{ p$sd }})
                    dnorm(x, mean = !!p$mean, sd = !!p$sd)
                )
            ),
            M = p$M,
            pdf = pdf
        ),
        Gamma = list(
            propose = new_function(
                pairlist2(k = ),
                expr(
                    # rgamma(k, shape = {{ p$shape }}, scale = {{ p$scale }})
                    rgamma(k, shape = !!p$shape, scale = !!p$scale)
                )
            ),
            density = new_function(
                pairlist2(x = ),
                expr(
                    # dgamma(x, shape = {{ p$shape }}, scale = {{ p$scale }})
                    dgamma(x, shape = !!p$shape, scale = !!p$scale)
                )
            ),
            M = p$M,
            pdf = pdf
        )
    )
}
