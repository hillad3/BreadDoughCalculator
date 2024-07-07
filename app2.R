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
      "Volumetric Conversion Helper",
      br(),
      accordion(
        open = TRUE,
        multiple = FALSE,
        accordion_panel(
          title = "Filter Ingredient Selector",
          column(
            width = 12,
            checkboxGroupInput(
              inputId = "tags",
              label = NULL,
              inline = TRUE,
              choiceNames = names(ingredient_tags),
              choiceValues = ingredient_tags |> unname(),
              selected = ingredient_tags
            ),
            actionButton(
              inputId = "no_tags",
              label = "Remove All"
            ),
            actionButton(
              inputId = "all_tags",
              label = "Include All"
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          tags$strong("Ingredient Selector"),
          style = "text-align:right"
        ),
        column(
          width = 6,
          selectInput(
            inputId = "ingredient",
            label = NULL,
            choices = NULL, # the reactive in the server will provide this after loading
            selected = NULL,
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          actionButton(
           inputId = "reset_page",
           label = "Reset",
           class = "btn btn-warning",
           style = "font-weight:bold;"
          )
        )
      ),
      br(),
      fluidRow(
        tags$strong("Volume: "),
        column(
          width = 2,
          numericInput(
            inputId = "vol_amt_in",
            label = "Amount",
            value = 0,
            min = 0,
            step = 1/8
          )
        ),
        column(
          width = 1,
          p("Lock"),
          checkboxInput(
            inputId = "amt_locked",
            label = NULL,
            value = FALSE
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = "vol_uom_in",
            label = "Unit of Measure",
            choices = c("cup","tablespoon","teaspoon"),
            selected = NA_character_,
            multiple = FALSE
          )
        ),
        column(
          width = 1,
          p("Lock"),
          checkboxInput(
            inputId = "uom_locked",
            label = NULL,
            value = FALSE
          )
        ),
        column(
          width = 2,
          actionButton(
            inputId = "calc_vol",
            label = "Calculate Volume",
            class = "btn btn-secondary",
            style = "margin-top:32px; margin-right:1%; font-weight:bold; font-size:10pt"
          )
        ),
        style="margin-left:1%"
      ),
      br(),
      fluidRow(
        tags$strong("Weight: "),
        column(
          width = 2,
          numericInput(
            inputId = "target_amt_in",
            label = "Amount",
            value = NA_real_,
            min = 0,
            step = 1/8
          )
        ),
        column(width = 1),
        column(
          width = 4,
          selectInput(
            inputId = "target_uom_in",
            label = "Unit of Measure",
            choices = c("ounce","gram"),
            selected = "gram",
            multiple = FALSE
          )
        ),
        column(width = 1),
        column(
          width = 2,
          actionButton(
            inputId = "calc_weight",
            label = "Calculate Weight",
            class = "btn btn-primary",
            style = "margin-top:32px; margin-right:1%; font-weight:bold; font-size:10pt"
          )
        ),
        style="margin-left:1%"
      ),
      br(),
      accordion(
        open = FALSE,
        multiple = FALSE,
        accordion_panel(
          "Table of Ingredients and Conversions",
          p(
            tags$span("Note: Weights shown below are"),
            tags$span("per 1 cup", style="font-weight:bold"),
            tags$span(" of ingredient. "),
            tags$span(" Original conversions adapted from the King Arthur Baking"),
            tags$a(href="https://www.kingarthurbaking.com/learn/ingredient-weight-chart", "website", noWS="after", target="_blank"),
            tags$span(".")
          ),
          DTOutput("full_table")
        )
      ),
      br()
    ) # close nav_panel
  )
)

server <- function(input, output, session) {

  # this is the primary output of the application
  observeEvent(
    input$calc_weight,
    handlerExpr = {

      updateNumericInput(
        inputId = "target_amt_in",
        value = input$vol_amt_in * vol_to_vol_conversion() * selected_weight_of_1cup()
      )
    }
  )

  # this is an alternative use case of the application
  observeEvent(
    input$calc_vol,
    handlerExpr = {

      updateNumericInput(
        inputId = "vol_amt_in",
        value = input$target_amt_in / vol_to_vol_conversion() / selected_weight_of_1cup()
      )
    }
  )

  # collect input from UI and associated inputs
  selected_ingredient <- reactive(input$ingredient)
  selected_target_uom <- reactive(input$target_uom_in)
  selected_default_vol_uom <- reactive({
    vol_conv[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$default_vol_uom
  })
  selected_default_conv_factor <- reactive({
    vol_conv[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$vol_conv_factor_to_cups
  })
  selected_weight_of_1cup <- reactive({
    vol_conv[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$target_amt
  })

  # use Non-standard Evaluation to create a string of true tags and parse in the data.table
  updated_ingredient_list <- reactive({

    selected_tags <- paste0(input$tags, collapse = " | ")

    selected_ingredients <- vol_conv[eval(parse_expr(selected_tags))==TRUE]

    selected_ingredients <- selected_ingredients[,.SD,.SDcols=c("ingredient")] |>
      unlist() |>
      unique() |>
      sort()

    return(selected_ingredients)

  })

  # update the ingredient list based on the tags selected
  observeEvent(
    input$tags,
    handlerExpr = {
      updateSelectInput(
        inputId = "ingredient",
        choices = updated_ingredient_list(),
        selected = updated_ingredient_list()[1]
      )
    }
  )

  # deselect all tags. Note: An empty input here will not return any value so also need to remove
  # the choices from the ingredient selector until the user selects a tag
  observeEvent(
    input$no_tags,
    handlerExpr = {
      updateCheckboxGroupInput(
        session = session,
        inputId = "tags",
        selected = NA
      )

      updateSelectInput(
        inputId = "ingredient",
        choices = list(),
        selected = NA_character_
      )
    }
  )

  # select all tags
  observeEvent(
    input$all_tags,
    handlerExpr = {
      updateCheckboxGroupInput(
        session = session,
        inputId = "tags",
        selected = ingredient_tags |> unname()
      )
    }
  )

  # determine vol-to-vol conversion for default UOM in vol_conv
  vol_to_vol_conversion <- reactive({
    # 16 TBSP in a cup
    # 3 TSP in a TBSP
    if(input$vol_uom_in=="cup"){
      1
    } else if(input$vol_uom_in=="tablespoon"){
      1/16
    } else if(input$vol_uom_in=="teaspoon"){
      1/16/3
    } else {
      stop(paste0("A conversion factor has not been established for a UOM of ",input$vol_uom_in))
    }
  })

  # update the volume inputs based on the ingredient selected, depending on locks used
  observeEvent(
    input$ingredient,
    handlerExpr = {

      if(!input$amt_locked) {
        updateNumericInput(
          inputId = "vol_amt_in",
          value = vol_conv[ingredient==selected_ingredient() & target_uom==selected_target_uom(),default_vol_amt] |> unlist()
        )
      } else {
        # do nothing, so it is easier to investigate weights between similar types of ingredients
      }

      if(!input$uom_locked) {
        updateSelectInput(
          inputId = "vol_uom_in",
          selected = vol_conv[ingredient==selected_ingredient() & target_uom==selected_target_uom(),default_vol_uom] |> unlist()
        )
      } else {
        # do nothing, so it is easier to investigate weights between similar types of ingredients
      }

    }
  )

  # reset the page
  observeEvent(
    input$reset_page,
    handlerExpr = {

      updateCheckboxGroupInput(
        session = session,
        inputId = "tags",
        selected = ingredient_tags |> unname()
      )

      updateSelectInput(
        inputId = "ingredient",
        choices = updated_ingredient_list(),
        selected = updated_ingredient_list()[1]
      )

      updateNumericInput(
        inputId = "vol_amt_in",
        value = NA_real_
      )

      updateCheckboxInput(
        inputId = "amt_locked",
        value = FALSE
      )

      updateSelectInput(
        inputId = "vol_uom_in",
        selected = NA_character_
      )

      updateCheckboxInput(
        inputId = "uom_locked",
        value = FALSE
      )

      updateNumericInput(
        inputId = "target_amt_in",
        value = NA_real_
      )

      updateSelectInput(
        inputId = "target_uom_in",
        selected = "gram"
      )
    }
  )

  # render full ingredient table
  output$full_table <- renderDT({

    dt <- vol_conv[,.(ingredient,target_uom,target_amt)]
    dt[,target_uom:=ifelse(target_uom=="gram","Grams","Ounces")]
    dt[,target_amt:=sprintf("%.3f",target_amt)]
    dt <- dcast(dt, ingredient ~ target_uom, value.var = "target_amt")
    setnames(dt, old = c("ingredient"), new = c("Ingredient"))

    DT::datatable(
      dt,
      options = list(
        autoWidth=TRUE,
        pageLength=10,
        language = list(search = 'Search:')
      )
    )
  })


}

shinyApp(ui = ui, server = server)
