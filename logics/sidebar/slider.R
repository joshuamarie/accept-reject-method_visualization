box::use(
    shiny[sliderInput, tagList, textInput, verbatimTextOutput, tags]
)

ui = tagList(
    sliderInput(
        "sample_size",
        "Sample size:",
        min = 0,
        max = 1e4,
        value = 1000, step = 50
    ),
    textInput("pdf", "Enter the PDF from which you want to sample:", "1 - abs(x)"),
    tags$h6("For example,enter 1/(2*0.5)*exp(-abs(x)/0.5) to sample from the Laplace distribution with location parameter = 0 and scale = 0.5"),
    verbatimTextOutput("value"),
    tags$b("Below you can select a proposal distribution")
)
