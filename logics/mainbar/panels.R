box::use(
    shiny[
        tagList, tags, plotOutput, tableOutput, sliderInput,
        textInput, downloadButton, actionButton, icon
    ],
    bslib[card, card_body, navset_tab, nav_panel]
)

#' @export
ui = function(ns) {
    card(
        full_screen = TRUE,
        card_body(
            navset_tab(
                id = ns("main_tabs"),
                nav_panel(
                    title = tagList(icon("circle-dot"), "Accept-Reject"),
                    value = "accept_reject",
                    plotOutput(ns("plot"), height = "500px")
                ),
                nav_panel(
                    title = tagList(icon("chart-area"), "Density"),
                    value = "density",
                    plotOutput(ns("plot2"), height = "500px")
                ),
                nav_panel(
                    title = tagList(icon("table-list"), "Summary"),
                    value = "summary",
                    tableOutput(ns("summary"))
                )
            ),
            actionButton(
                ns("go"),
                label = tagList(icon("play"), "Sample new data"),
                class = "btn btn-primary btn-sample w-100 mt-2"
            ),
            sliderInput(
                ns("plotrange"),
                "x-axis range",
                min = -10,
                max = 10,
                value = c(-2, 2),
                step = 0.1
            ),
            tags$div(
                class = "d-flex gap-2 align-items-center",
                textInput(
                    ns("filename"),
                    label = NULL,
                    placeholder = "filename",
                    value = "filename"
                ),
                downloadButton(
                    ns("downloadData"),
                    "Download CSV",
                    class = "btn btn-outline-primary btn-sm"
                )
            )
        )
    )
}
