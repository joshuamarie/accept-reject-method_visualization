box::use(
    shiny[tagList, tags, numericInput, sliderInput],
    bslib[navset_tab, nav_panel]
)

#' @export
ui = function(ns) {
    tagList(
        tags$p(class = "fw-semibold mb-2", "Proposal distribution"),
        navset_tab(
            id = ns("tabset"),
            nav_panel(
                "Uniform",
                numericInput(ns("Munif"), "Blow-up factor M", 2),
                sliderInput(
                    ns("uni_range"),
                    "Range",
                    min = -10,
                    max = 10,
                    value = c(-1, 1)
                )
            ),
            nav_panel(
                "Gaussian",
                numericInput(ns("Mnormal"), "Blow-up factor M", 1),
                numericInput(ns("mu"), "Mean", 0),
                numericInput(ns("sigma"), "Std. deviation", 1)
            ),
            nav_panel(
                "Gamma",
                numericInput(ns("Mgamma"), "Blow-up factor M", 1),
                numericInput(ns("shape"), "Shape", 1),
                numericInput(ns("scale"), "Scale", 1)
            )
        ),
        tags$hr()
    )
}
