box::use(
    shiny[shinyApp, tags],
    bslib[bs_theme, font_google],
    ./logics[logic_ui = ui, logic_server = server]
)

theme = bs_theme(
    version = 5,
    primary = "#2166ac",
    secondary = "#6c757d",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("Fira Code")
)

ui = bslib::page_fluid(
    theme = theme,
    tags$head(
        tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&family=Fira+Code&display=swap"
        ),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    logic_ui("sampler")
)

server = function(input, output, session) {
    logic_server("sampler")
}

shinyApp(ui, server)
