box::use(
    shiny[
        NS, moduleServer, eventReactive, renderPlot, renderUI,
        renderTable, downloadHandler, req, tagList, sliderInput,
        uiOutput, textInput, tags, numericInput, plotOutput,
        tableOutput, downloadButton, icon, actionButton
    ],
    bslib[
        layout_sidebar, sidebar, card, card_body,
        navset_tab, nav_panel, input_dark_mode
    ],
    dplyr[keep_when = filter, mutate, tbl = tibble],
    ggplot2[
        ggplot, aes, geom_point, geom_density, scale_color_manual,
        labs, theme_minimal, theme, element_text, element_rect, margin,
        coord_cartesian
    ],
    readr[write_csv],
    scales[comma],

    ./core/sampler,
    ./core/params,
    pdf_parser = ./core/pdf_parser
)

ar_theme = function() {
    theme_minimal(base_size = 13) +
        theme(
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            legend.position = "top",
            legend.text = element_text(size = 11),
            plot.margin = margin(8, 16, 8, 8)
        )
}

#' @export
ui = function(id) {
    ns = NS(id)

    layout_sidebar(
        sidebar = sidebar(
            width = 300,
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
            tags$hr(),
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
        ),
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

        output$kpi_accepted = renderUI({
            if (is.null(sample_data())) {
                tags$span(class = "kpi-value kpi-pending", "—")
            } else {
                n = nrow(keep_when(sample_data(), status == "accepted"))
                tags$span(class = "kpi-value", comma(n))
            }
        })

        output$kpi_ratio = renderUI({
            if (is.null(sample_data())) {
                tags$span(class = "kpi-value kpi-pending", "—")
            } else {
                dat = sample_data()
                n_ac = nrow(keep_when(dat, status == "accepted"))
                n_rj = nrow(keep_when(dat, status == "rejected"))
                tags$span(class = "kpi-value", round(n_ac / max(n_rj, 1), 2))
            }
        })

        output$plot = renderPlot(bg = "transparent", {
            req(sample_data())
            ggplot(sample_data(), aes(x = x, y = y, color = status)) +
                geom_point(alpha = 0.45, size = 0.9) +
                scale_color_manual(
                    values = c("accepted" = "#2166ac", "rejected" = "#d6604d")
                ) +
                coord_cartesian(xlim = input$plotrange) +
                labs(x = NULL, y = NULL, color = NULL) +
                ar_theme()
        })

        output$plot2 = renderPlot(bg = "transparent", {
            req(sample_data())
            ggplot(
                keep_when(sample_data(), status == "accepted"),
                aes(x = x)
            ) +
                geom_density(fill = "#2166ac", alpha = 0.35, color = "#2166ac") +
                coord_cartesian(xlim = input$plotrange) +
                labs(title = "Density of accepted samples", x = NULL, y = NULL) +
                ar_theme()
        })

        output$summary = renderTable(
            {
                req(sample_data())
                dat = sample_data()
                accepted = keep_when(dat, status == "accepted")$x
                n_rj = nrow(keep_when(dat, status == "rejected"))
                s = summary(accepted)
                tbl(
                    Statistic = c(names(s), "Accept/Reject Ratio"),
                    Value = c(as.numeric(s), length(accepted) / n_rj)
                )
            },
            striped = TRUE,
            hover = TRUE,
            bordered = FALSE
        )

        output$downloadData = downloadHandler(
            filename = function() paste0(input$filename, ".csv"),
            content = function(file) {
                write_csv(
                    keep_when(sample_data(), status == "accepted")["x"],
                    file
                )
            }
        )
    })
}
