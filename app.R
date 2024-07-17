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
library(rlang)

source("moduleVolumeConversion.R")

vol_conv <- fread("King Arthur Volumetric Conversions - Cleaned.csv")

ingredient_tags <- names(vol_conv)[grepl("tag",names(vol_conv))]
names(ingredient_tags) <- str_to_title(sub("_tag","",ingredient_tags))

main_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  bg = "#1d1d1d",
  fg = "white"
  # base_font = "Helvetica"
)

ui <- page_fluid(
  theme = main_theme,
  tags$h1("Bread Dough Calculator", style="padding-top:10px;"),
  navset_underline(
    nav_panel(
      title = "Volume-to-Mass Conversions",
      modVolumeConversionUI("nav_panel1", vol_conv, ingredient_tags)
    ),
    nav_panel(
      title = "Hydration Calculator",
      br(),
      tags$h1("Under development")
    )
  )
)

server <- function(input, output, session) {

  modVolumeConversionServer("nav_panel1", vol_conv, ingredient_tags)

}

shinyApp(ui = ui, server = server)
