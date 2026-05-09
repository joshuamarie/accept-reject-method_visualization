box::use(
    shiny[tagList, tabsetPanel, sliderInput, tabPanel, numericInput]
)

ui = tagList(
    tabsetPanel(
        id = "tabset",
        tabPanel(
            "Uniform",
            numericInput("Munif", "Blow-up factor (M)", 2),
            sliderInput("uni_range", "Range of Uniform distribution", min = -10, max = 10, value = c(-1, 1))
        ),
        tabPanel(
            "Gaussian",
            numericInput("Mnormal", "Blow-up factor", 1),
            numericInput("mu", "Mean", 0),
            numericInput("sigma", "Standard Deviation", 1)
        ),
        tabPanel(
            "Gamma",
            numericInput("Mgamma", "Blow-up factor", 1),
            numericInput("shape", "shape", 1),
            numericInput("scale", "scale", 1)
        ),
        type = "tabs"
    )
)
