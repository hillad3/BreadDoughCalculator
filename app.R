rm(list = ls())
gc()

library(shiny)
library(bslib)
library(data.table)
library(DT)
library(tibble)
library(dplyr)
library(stringr)
library(logger)

source("moduleVolumeConversion.R")

vol_conv <- fread("King Arthur Volumetric Conversions - Cleaned.csv")
ingredient_classes <- c("", vol_conv |>
  select(contains("tag")) |>
  names() |>
  str_remove("_tag") |>
  str_to_title() |> sort())

main_theme <- bs_theme(
  version = 5,
  # bg = "#222222",
  # fg = "#FFFFFF",
  # base_font = "Helvetica"
)

ui <- page_fluid(
  theme = main_theme,
  tags$h1("Bread Dough Calculator", style="padding-top:10px;"),
  navset_underline(
        nav_panel(
      "Volumetric Conversion Helper",
      modVolumeConversionUI("panel_2",vol_conv, ingredient_classes)
    ), # close nav_panel
    nav_panel(
      "Formula Builder",
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
      ),
      # modVolumeConversionUI("",vol_conv)
    ), # close nav_panel
    nav_panel(
      "About"
    )
  )
)

server <- function(input, output, session) {

  modVolumeConversionServer("panel_2", vol_conv, ingredient_classes)

}

shinyApp(ui = ui, server = server)



# output$buttons <- renderUI({
#   tagList(
#     lapply(seq_len(input$n), function(i) {
#       # create a button using tags manually to be able to set onclick
#       tags$button(
#         id = paste0("button", i),
#         type = "button",
#         class = "btn btn-default action-button",
#         # magic sauce here
#         onclick = sprintf("Shiny.setInputValue('%s', %s)", "click_button", i), # in a module use ns()!
#         paste("Button", i))
#     })
#   )
# })
#
