box::use(
    shiny[NS, tagList, tags, sliderInput, textInput, uiOutput],
    bslib[input_dark_mode]
)

#' @export
ui = function(ns) {
    tagList(
        tags$div(
            class = "d-flex justify-content-between align-items-center mb-3",
            tags$h5(class = "mb-0 fw-bold", "A/R Sampling Visualizer"),
            input_dark_mode(id = ns("dark_mode"), mode = "light")
        ),
        tags$div(
            class = "kpi-grid mb-3",
            tags$div(
                class = "kpi-box",
                tags$div(class = "kpi-label", "Accepted"),
                uiOutput(ns("kpi_accepted"))
            ),
            tags$div(
                class = "kpi-box",
                tags$div(class = "kpi-label", "A/R Ratio"),
                uiOutput(ns("kpi_ratio"))
            )
        ),
        tags$hr(),
        sliderInput(
            ns("sample_size"),
            "Sample size",
            min = 0,
            max = 1e4,
            value = 1000,
            step = 50
        ),
        tags$hr(),
        textInput(
            ns("pdf"),
            "Target PDF f(x)",
            value = "1 - abs(x)"
        ),
        tags$small(
            class = "text-muted",
            "e.g. ", tags$code("1/(2*0.5)*exp(-abs(x)/0.5)"),
            tags$br(), "Laplace(0, 0.5)"
        ),
        tags$hr()
    )
}
