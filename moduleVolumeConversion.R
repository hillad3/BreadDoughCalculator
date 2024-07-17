
modVolumeConversionUI <- function(id_,
                                  vol_conv_,
                                  ingredient_tags_){
  tagList(
    br(),
    accordion(
      open = TRUE,
      multiple = FALSE,
      accordion_panel(
        title = "Filter Ingredient Selector",
        column(
          width = 12,
          checkboxGroupInput(
            inputId = NS(id_,"tags"),
            label = NULL,
            inline = TRUE,
            choiceNames = names(ingredient_tags_),
            choiceValues = ingredient_tags_ |> unname(),
            selected = ingredient_tags_
          ),
          actionButton(
            inputId = NS(id_,"no_tags"),
            label = "Remove All"
          ),
          actionButton(
            inputId = NS(id_,"all_tags"),
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
          inputId = NS(id_,"ingredient"),
          label = NULL,
          choices = NULL, # the reactive in the server will provide this after loading
          selected = NULL,
          multiple = FALSE
        )
      ),
      column(
        width = 3,
        actionButton(
          inputId = NS(id_,"reset_page"),
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
          inputId = NS(id_,"vol_amt_in"),
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
          inputId = NS(id_,"amt_locked"),
          label = NULL,
          value = FALSE
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = NS(id_,"vol_uom_in"),
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
          inputId = NS(id_,"uom_locked"),
          label = NULL,
          value = FALSE
        )
      ),
      column(
        width = 2,
        actionButton(
          inputId = NS(id_,"calc_vol"),
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
          inputId = NS(id_,"target_amt_in"),
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
          inputId = NS(id_,"target_uom_in"),
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
          inputId = NS(id_,"calc_weight"),
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
  )
}

modVolumeConversionServer <- function(id_, vol_conv_, ingredient_tags_){
  moduleServer(
    id_,
    function(input, output, session){

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
        vol_conv_[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$default_vol_uom
      })
      selected_default_conv_factor <- reactive({
        vol_conv_[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$vol_conv_factor_to_cups
      })
      selected_weight_of_1cup <- reactive({
        vol_conv_[ingredient==selected_ingredient() & target_uom==selected_target_uom()]$target_amt
      })

      # use Non-standard Evaluation to create a string of true tags and parse in the data.table
      updated_ingredient_list <- reactive({

        selected_tags <- paste0(input$tags, collapse = " | ")

        selected_ingredients <- vol_conv_[eval(parse_expr(selected_tags))==TRUE]

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
            selected = ingredient_tags_ |> unname()
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
              value = vol_conv_[ingredient==selected_ingredient() & target_uom==selected_target_uom(),default_vol_amt] |> unlist()
            )
          } else {
            # do nothing, so it is easier to investigate weights between similar types of ingredients
          }

          if(!input$uom_locked) {
            updateSelectInput(
              inputId = "vol_uom_in",
              selected = vol_conv_[ingredient==selected_ingredient() & target_uom==selected_target_uom(),default_vol_uom] |> unlist()
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
            selected = ingredient_tags_ |> unname()
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

        dt <- vol_conv_[,.(ingredient,target_uom,target_amt)]
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
  )
}