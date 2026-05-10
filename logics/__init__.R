box::use(
    shiny[
        NS, moduleServer, eventReactive, renderPlot, renderUI,
        renderTable, downloadHandler, req, tagList, tags
    ],
    bslib[
        layout_sidebar, sidebar
    ],
    dplyr[keep_when = filter, mutate, tbl = tibble],
    ggtext[element_markdown],
    ggplot2[
        ggplot, aes, geom_point, geom_density, scale_color_manual,
        labs, theme_minimal, theme, element_text, element_rect, margin,
        coord_cartesian, element_blank, element_line, scale_shape_manual
    ],
    readr[write_csv],
    scales[comma],

    ./mainbar/panels,
    ./sidebar/slider,
    ./sidebar/tabpanels,
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
        # ---- Sidebar Content ----
        sidebar = sidebar(
            width = 300,
            slider$ui(ns),
            tabpanels$ui(ns)
        ),
        # ---- Main Panel ----
        panels$ui(ns)
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
            fg = session$clientData[["output_sampler-plot_fg"]]
            fg = if (is.null(fg)) "black" else {
                vals = as.integer(regmatches(fg, gregexpr("[0-9]+", fg))[[1]])
                grDevices::rgb(vals[1], vals[2], vals[3], maxColorValue = 255)
            }

            ggplot(sample_data(), aes(x = x, y = y, color = status)) +
                geom_point(aes(shape = status), alpha = 0.97, size = 3.5, stroke = 1.3) +
                scale_color_manual(
                    values = c(
                        "accepted" = "#093C5D",
                        "rejected" = "#7F2020"
                    )
                ) +
                scale_shape_manual(
                    values = c(
                        "accepted" = "\u25CF",
                        "rejected" = "\u25A0"
                    )
                    # guide = "none"
                ) +
                coord_cartesian(xlim = input$plotrange) +
                labs(
                    title = "**Accept-Reject Sampling**",
                    subtitle = "<span style='color:#2166ac'>&#9679; accepted</span> &nbsp; <span style='color:#d6604d'>&#9632; rejected</span>",
                    x = NULL, y = NULL, color = NULL
                ) +
                ar_theme() +
                theme(
                    plot.title = element_markdown(size = 13, color = fg),
                    plot.subtitle = element_markdown(size = 11),
                    legend.position = "none",
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_line(linewidth = 0.3, color = "grey85")
                )
        })

        output$plot2 = renderPlot(bg = "transparent", {
            req(sample_data())
            fg = session$clientData[["output_sampler-plot_fg"]]
            fg = if (is.null(fg)) "black" else {
                vals = as.integer(regmatches(fg, gregexpr("[0-9]+", fg))[[1]])
                grDevices::rgb(vals[1], vals[2], vals[3], maxColorValue = 255)
            }

            ggplot(
                keep_when(sample_data(), status == "accepted"),
                aes(x = x)
            ) +
                geom_density(fill = "#2166ac", alpha = 0.2, color = "#2166ac", linewidth = 0.8) +
                coord_cartesian(xlim = input$plotrange) +
                labs(title = "**Density** of accepted samples", x = NULL, y = NULL) +
                ar_theme() +
                theme(
                    plot.title = element_markdown(size = 13, color = fg),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_line(linewidth = 0.3, color = "grey85")
                )
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
