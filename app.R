library(shiny)
library(bslib)
library(data.table)

main_theme <- bs_theme(
  version = 5,
  bg = "#222222",
  fg = "#FFFFFF",
  base_font = "Helvetica"
)

ui <- page_fluid(
  theme = main_theme,
  tags$h1("Bread Dough Calculator", style="padding-top:10px;"),
  fluidRow(
    column(width = 10),
    column(
      width = 2,
      selectInput(
        inputId = "unit_of_measure",
        label = "Units",
        selected = "g",
        choices = c("g","oz"),
        multiple = FALSE
      )
    )
  ),
  accordion(
    open = FALSE,
    multiple = FALSE,
    accordion_panel(
      title = "Levain",
      open = FALSE,
      multiple = FALSE,
      textInput(
        inputId = "starter_weight",
        label = "Weight",
        width = "100px",
        value="50"
      )
    )
  )
)

server <- function(input, output, session) {


}

shinyApp(ui = ui, server = server)