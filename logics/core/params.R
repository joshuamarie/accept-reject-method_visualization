box::use(
    rlang[list2]
)

#' Create a typed parameter bundle for a single proposal distribution
#'
#' @param ... Named distribution parameters (e.g. min=, max=, mean=, shape=)
#' @param M Numeric. Blow-up factor.
#'
#' @return A named list with class "dist_components"
#' @export
components = function(..., M) {
    structure(
        list2(..., M = M),
        class = "dist_components"
    )
}

#' Collect all proposal distribution parameter bundles
#'
#' @param uniform A dist_components object for the Uniform proposal.
#' @param normal A dist_components object for the Normal proposal.
#' @param gamma A dist_components object for the Gamma proposal.
#'
#' @return A named list with class "known_dists"
#' @export
known_dists = function(uniform, normal, gamma) {
    structure(
        list2(
            uniform = uniform,
            normal = normal,
            gamma = gamma
        ),
        class = "known_dists"
    )
}
