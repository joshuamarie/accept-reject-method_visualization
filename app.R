box::use(
    shiny[shinyApp, tags, includeMarkdown],
    bslib[
        bs_theme, font_google, page_navbar, nav_panel,
        nav_spacer, nav_item, input_dark_mode, navbar_options
    ],
    ./logics[logic_ui = ui, logic_server = server]
)

theme = bs_theme(
    version = 5,
    primary = "#2166ac",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("Fira Code")
)

ui = page_navbar(
    title = "A/R Sampling Visualizer",
    theme = theme,
    navbar_options = navbar_options(bg = "#215B63"),
    header = tags$head(
        tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&family=Fira+Code&display=swap"
        ),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    nav_panel(
        "Home",
        logic_ui("sampler")
    ),
    nav_panel(
        "About",
        tags$div(
            class = "container py-4",
            includeMarkdown("README.md")
        )
    ),
    nav_spacer(),
    nav_item(
        input_dark_mode(id = "dark_mode", mode = "light")
    )
)

server = function(input, output, session) {
    logic_server("sampler")
}

shinyApp(ui, server)
