box::use(
    shiny[
        tabsetPanel, tabPanel, plotOutput, tableOutput, sliderInput,
        textInput, downloadButton, tagList
    ]
)

ui = tagList(
    tabsetPanel(
        tabPanel("Accept-Reject ratio", plotOutput("plot")),
        tabPanel("Summary Stats", tableOutput("summary")),
        tabPanel("Density of sampled values", plotOutput("plot2"))
    ),
    sliderInput("plotrange", "x-axis range", min = -10, max = 10, value = c(-2, 2), step = 0.1),
    textInput("filename", "You can save a csv file of the accepted values here:", "enter_file_name_here"),
    downloadButton("downloadData", "Download")
)
