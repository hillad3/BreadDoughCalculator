
modVolumeConversionUI <- function(id_,
                                  vol_conv_,
                                  ingredient_classes_){
  tagList(
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
          choices = c(vol_conv_$ingredient),
          multiple = FALSE
        )
      ),
      column(width = 3,
        actionButton(
          inputId = NS(id_,"reset_page"),
          label = "Reset",
          style = "color:white; font-type:bold;"
        )
      )
    ),
    br(),
    fluidRow(
      tags$strong("Volume (Input): "),
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
        width = 4,
        selectInput(
          inputId = NS(id_,"vol_uom_in"),
          label = "Unit of Measure",
          choices = c("","cup","tablespoon","teaspoon"),
          selected = "cup",
          multiple = FALSE
        )
      ),
      style="margin-left:1%"
    ),
    br(),
    fluidRow(
      tags$strong("Weight (Output): "),
      column(
        width = 2,
        numericInput(
          inputId = NS(id_,"target_amt_in"),
          label = "Amount",
          value = 0,
          min = 0,
          step = 1/8
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = NS(id_,"target_uom_in"),
          label = "Unit of Measure",
          choices = c("","ounce","gram"),
          selected = "gram",
          multiple = FALSE
        )
      ),
      style="margin-left:1%"
    ),
    br(),
    accordion(
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        title = "Filter Ingredient List",
        selectInput(
          inputId = NS(id_,"ingred_classes"),
          label = NULL,
          choices = ingredient_classes_,
          selected = ingredient_classes_ |> as.list(),
          multiple = TRUE
        ),
        actionButton(
          inputId = NS(id_,"no_classes"),
          label = "Remove All"
        ),
        actionButton(
          inputId = NS(id_,"all_classes"),
          label = "Include All"
        )
      )
    ),
    accordion(
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        "Table of Ingredients and Conversions",
        p("Note: Weights shown below are per 1 cup of ingredient."),
        DTOutput(NS(id_,"full_table")),
      )
    ),
    br()
  )
}

modVolumeConversionServer <- function(id_, vol_conv_, ingredient_classes_){
  moduleServer(
    id_,
    function(input, output, session){

      selected_ingredient <- reactive(input$ingredient)

      observeEvent(
        input$ingredient,
        handlerExpr = {
          updateNumericInput(
            inputId = "vol_amt_in",
            value = as.numeric(vol_conv_[ingredient==selected_ingredient(),default_vol_amt] |> unlist())
          )

          updateSelectInput(
            inputId = "vol_uom_in",
            selected = vol_conv_[ingredient==selected_ingredient(),default_vol_unit] |> unlist()
          )
        }
      )

      observeEvent(
        input$reset_page,
        handlerExpr = {
          updateSelectInput(
            session = session,
            inputId = "ingredient",
            selected = NULL,
          )
        }
      )

      observeEvent(
        input$no_classes,
        handlerExpr = {
          updateSelectInput(
            session = session,
            inputId = "ingred_classes",
            selected = NULL,
          )
        }
      )

      observeEvent(
        input$all_classes,
        handlerExpr = {
          updateSelectInput(
            session = session,
            inputId = "ingred_classes",
            selected = ingredient_classes_ |> as.list(),
          )
        }
      )


      # render full ingredient table
      output$full_table <- renderDT({

        dt <- vol_conv_[,.(ingredient,target_uom,target_quantity)]
        dt[,target_uom:=ifelse(target_uom=="gram","Grams","Ounces")]
        dt[,target_quantity:=sprintf("%.3f",target_quantity)]
        dt <- dcast(dt, ingredient ~ target_uom, value.var = "target_quantity")
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


# re <- reactive(
#   if(is.null(input$regex_str) | input$regex_str==""){
#     "."
#   } else {
#     str_to_upper(input$regex_str)
#   }
# )
#
# dt <- reactive({
#
#   # logger::log_info(re())
#
#   dt <- tryCatch(
#     expr = {dt_words_[str_detect(Word, re())]},
#     error = function(e){dt_words_[Word=="12345"]}
#   )
#
#   if(!input$include_scrabble_dict){
#     dt <- dt[(Date >= input$date_range[1] & Date <= input$date_range[2])]
#   } else {
#     dt <- dt[is.na(Date) | (Date >= input$date_range[1] & Date <= input$date_range[2])]
#   }
#
#   if(input$include_scrabble_dict){
#     dt
#   } else {
#     dt <- dt[!is.na(Date)]
#     setorder(dt, -Index)
#   }
#
#   if(input$check_dups){
#     dt <- dt[duplicated(Word)]
#   }
#
#   dt
#
# })
#
# output$word_list_table <- renderDT(
#
#   if(dim(dt())[1]==0L){
#     data.table(" " = "There are no historical answers with the filtered parameters!")
#   } else {
#
#     if("Date" %in% names(dt())){
#       setnames(dt(), old = c("Date","Index"), new=c("Answer Date (YYYY-MM-DD)","Wordle Index"))
#     }
#
#     DT::datatable(
#       dt(),
#       options = list(
#         autoWidth=TRUE,
#         pageLength=10,
#         language = list(search = 'Normal Word Search:')
#       )) |>
#       formatStyle(columns = names(dt())[1], color = "#EDC001") |>
#       formatStyle(columns = names(dt())[2], color = "#3BC143")
#   }
#
# )
#
# output$letter_freq <- renderPlotly({
#   letter_counts <- dt() |>
#     unnest_tokens(char,Word,"characters") |>
#     mutate(
#       char = toupper(char),
#       position = forcats::fct(
#         rep(c("1st","2nd","3rd","4th","5th"), dim(dt())[1]),
#         c("5th","4th","3rd","2nd","1st")
#       )
#     ) |>
#     group_by(char, position) |>
#     reframe(n = n())
#
#   if(dim(letter_counts)[1]>0){
#     plot_ly(letter_counts, x = ~char, y = ~n, color = ~position, type = "bar") |>
#       layout(
#         barmode = "stack",
#         yaxis = list(title = "Frequency of Occurance", color = "#CCCCCC"),
#         xaxis = list(title = "Letters", color = "#CCCCCC"),
#         legend = list(title = list(text="<b>Letter Position</b>", font = list(color="#CCCCCC")),
#                       font = list(color="#CCCCCC")),
#         paper_bgcolor = "#363636",
#         plot_bgcolor = "#363636"
#       )
#   }
#
# })
#
#
# observeEvent(
#   input$year_filter,
#   if(input$year_filter==""){
#     updateDateRangeInput(
#       inputId = "date_range",
#       start = "2021-06-19",
#       end = max_date_
#     )
#   } else {
#     updateDateRangeInput(
#       inputId = "date_range",
#       start = paste0(input$year_filter,"-01-01"),
#       end = paste0(input$year_filter,"-12-31")
#     )
#   }
# )
#