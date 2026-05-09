box::use(
    shiny[
        NS, actionButton, moduleServer, eventReactive, renderPlot,
        renderTable, downloadHandler, req, tagList, sliderInput,
        textInput, tabsetPanel, tags, tabPanel, numericInput,
        plotOutput, tableOutput, downloadButton, sidebarLayout,
        sidebarPanel, mainPanel
    ],
    dplyr[keep_when = filter, mutate, tbl = tibble],
    ggplot2[
        ggplot, aes, geom_point, geom_density, scale_color_manual,
        labs, theme_minimal
    ],
    readr[write_csv],

    ./core/sampler,
    ./core/params,
    pdf_parser = ./core/pdf_parser
)

#' @export
ui = function(id) {
    ns = NS(id)
    tagList(
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                sliderInput(
                    ns("sample_size"), "Sample size:",
                    min = 0, max = 1e4, value = 1000, step = 50
                ),
                textInput(
                    ns("pdf"),
                    "Enter the PDF from which you want to sample:",
                    value = "1 - abs(x)"
                ),
                tags$h6(
                    "For example, enter 1/(2*0.5)*exp(-abs(x)/0.5) to sample",
                    "from the Laplace distribution with location = 0, scale = 0.5"
                ),
                tags$b("Below you can select a proposal distribution"),
                tabsetPanel(
                    id = ns("tabset"),
                    tabPanel(
                        "Uniform",
                        numericInput(ns("Munif"), "Blow-up factor (M)", 2),
                        sliderInput(ns("uni_range"), "Range of Uniform distribution",
                                    min = -10, max = 10, value = c(-1, 1))
                    ),
                    tabPanel(
                        "Gaussian",
                        numericInput(ns("Mnormal"), "Blow-up factor", 1),
                        numericInput(ns("mu"), "Mean", 0),
                        numericInput(ns("sigma"), "Standard Deviation", 1)
                    ),
                    tabPanel(
                        "Gamma",
                        numericInput(ns("Mgamma"), "Blow-up factor", 1),
                        numericInput(ns("shape"), "Shape", 1),
                        numericInput(ns("scale"), "Scale", 1)
                    )
                ),
                actionButton(ns("go"), "Sample new data"),
            ),
            mainPanel = mainPanel(
                tabsetPanel(
                    tabPanel("Accept-Reject ratio", plotOutput(ns("plot"))),
                    tabPanel("Summary Stats", tableOutput(ns("summary"))),
                    tabPanel("Density of sampled values", plotOutput(ns("plot2")))
                ),
                sliderInput(
                    ns("plotrange"), "x-axis range",
                    min = -10, max = 10, value = c(-2, 2), step = 0.1
                ),
                textInput(ns("filename"), "Save accepted values as CSV:", "filename"),
                downloadButton(ns("downloadData"), "Download")
            )
        )
    )
}

#' @export
server = function(id) {
    moduleServer(id, function(input, output, session) {
        sample_data = eventReactive(input$go, {
            pdf_fn = pdf_parser$parse_pdf(input$pdf)
            req(!is.null(pdf_fn))

            proposal = sampler$known_dist_metadata(
                tab = input$tabset,
                pdf = pdf_fn,
                params = params$known_dists(
                    uniform = params$components(
                        min = input$uni_range[1],
                        max = input$uni_range[2],
                        M = input$Munif
                    ),
                    normal = params$components(
                        mean = input$mu,
                        sd = input$sigma,
                        M = input$Mnormal
                    ),
                    gamma = params$components(
                        shape = input$shape,
                        scale = input$scale,
                        M = input$Mgamma
                    )
                )
            )

            sampler$sampler(n = input$sample_size, propose = proposal)
        })

        output$plot = renderPlot({
            req(sample_data())
            ggplot(sample_data(), aes(x = x, y = y, color = status)) +
                geom_point(alpha = 0.5, size = 0.8) +
                scale_color_manual(
                    values = c("accepted" = "#2166ac", "rejected" = "#d6604d")
                ) +
                labs(x = NULL, y = NULL, color = NULL) +
                theme_minimal()
        })

        output$plot2 = renderPlot({
            req(sample_data())
            ggplot(
                keep_when(sample_data(), status == "accepted"),
                aes(x = x)
            ) +
                geom_density(fill = "#2166ac", alpha = 0.4) +
                labs(title = "Density of sampled values", x = NULL, y = NULL) +
                theme_minimal()
        })

        output$summary = renderTable({
            req(sample_data())
            dat = sample_data()
            accepted = keep_when(dat, status == "accepted")$x
            n_rej = nrow(keep_when(dat, status == "rejected"))
            s = summary(accepted)

            tbl(
                Statistic = c(names(s), "Accept/Reject Ratio"),
                Value = c(as.numeric(s), length(accepted) / n_rej)
            )
        })

        output$downloadData = downloadHandler(
            filename = function() paste0(input$filename, ".csv"),
            content = function(file) {
                write_csv(keep_when(sample_data(), status == "accepted")["x"], file)
            }
        )
    })
}
