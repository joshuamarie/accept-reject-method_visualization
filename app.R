box::use(
    shiny[fluidPage, titlePanel, shinyApp],
    ./logics[logic_ui = ui, logic_server = server]
)

ui = fluidPage(
    titlePanel("Acceptance-Rejection Sampling"),
    logic_ui("sampler")
)

server = function(input, output, session) {
    logic_server("sampler")
}

shinyApp(ui, server)
