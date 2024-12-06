# Ok, let's begin....
app_ui <- function() {
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        HTML('<span style="color: white;">National Red Index</span>'), 
        style = "color: white; 
           text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;
           font-size: 20px; 
           font-family: 'Arial', cursive, sans-serif;"
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Front Page", tabName = "frontPage", icon = icon("home")),
        # menuItem("Frequencies", tabName = "Frequencies"),
        # menuItem("Percentages: 1 Group", tabName = "Proportions"),
        # menuItem("Percentages: 2 Groups", tabName = "Proportions2"),
        # menuItem("Averages: 1 Group", tabName = "Averages"),
        # menuItem("Averages: 2 Groups", tabName = "Averages2"),
        # menuItem("Social Tariff Machine", tabName = "Modeling"),
        # menuItem("Modeling: lower 2 expenditures", tabName = "Modeling2"),
        # menuItem("Modeling: increase 1 income", tabName = "Modeling3"),
        menuItem("Line: 1 group", tabName = "Lines"),
        menuItem("Line: 2 groups", tabName = "Lines2"),
        menuItem("Line: Negbud", tabName = "Lines3"),
        # menuItem("Line: Living on Empty", tabName = "Lines4"),
        menuItem("Bar: Simple", tabName = "Bars"),
        menuItem("Bar: Stacked", tabName = "Bars2"),
        menuItem("Bar: Grouped", tabName = "Bars3"),
        menuItem("Area: Stacked", tabName = "Areas"),
        menuItem("Area: %", tabName = "Areas2")
        # menuItem(text = "Maps", tabName = "Maps"),
        # menuItem(text = "Density", tabName = "Density")
      )
    ),
    dashboardBody(
      use_theme(my_theme),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      img(src = "logo.png", id = "logo"), 
      tabItems(
        # Front Page
        tabItem(
          tabName = "frontPage",
          fluidRow(
            column(
              6,
              uiOutput("variablePicker"), # Categorical variable selection
              uiOutput("levelPicker") # Level selection for chosen variable
            ),
            column(
              6,
              selectInput("datasetPicker", "Choose Dataset:",
                choices = c("Main", "April 24")
              ) # Dataset selection
            )
          ),
          actionButton("confirm", "Confirm Filter"),
          actionButton("reset", "Reset Filter"),
          verbatimTextOutput("filterStatus")
        ),
        # # Proportions page
        # tabItem(
        #   tabName = "Proportions",
        #   # h3("Proportions"),
        #   fluidRow(
        #     column(
        #       width = 6,
        #       selectInput(
        #         inputId = "Tab2Variable1",
        #         label = "Choose a time variable:",
        #         choices = c("Financial.Year")
        #       )
        #     ),
        #     column(
        #       width = 6,
        #       selectInput(
        #         inputId = "Tab2Variable2",
        #         label = "Choose a grouping variable:",
        #         choices = names(MART.Dash %>% select(starts_with("Demographic")))
        #       )
        #     )
        #   ),
        #   fluidRow(
        #     box(DT::dataTableOutput("table2"), width = 12)
        #   ),
        #   downloadButton("download2", "Download Data")
        # ),
        # # Proportions 2
        # tabItem(
        #   tabName = "Proportions2",
        #   # h3("Proportions"),
        #   fluidRow(
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab3Variable1",
        #         label = "Choose a time variable:",
        #         choices = c("Financial.Year")
        #       )
        #     ),
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab3Variable2",
        #         label = "Choose a grouping variable:",
        #         choices = names(MART.Dash %>% select(starts_with("Demographic")))
        #       )
        #     ),
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab3Variable3",
        #         label = "Choose a grouping variable:",
        #         choices = rev(names(MART.Dash %>% select(starts_with("Demographic"))))
        #       )
        #     )
        #   ),
        #   fluidRow(
        #     box(DT::dataTableOutput("table3"), width = 12)
        #   ),
        #   downloadButton("download3", "Download Data")
        # ),
        # # Summary stats 1
        # tabItem(
        #   tabName = "Averages",
        #   # h3("Averages"),
        #   fluidRow(
        #     column(
        #       width = 6,
        #       selectInput(
        #         inputId = "Tab4Variable1",
        #         label = "Choose a time variable:",
        #         choices = c("Financial.Year")
        #       )
        #     ),
        #     column(
        #       width = 6,
        #       selectInput(
        #         inputId = "Tab4Variable2",
        #         label = "Choose a numeric variable:",
        #         choices = names(MART.Dash %>% select(
        #           `Surplus`, starts_with("Expenditure"), starts_with("Income"),
        #           starts_with("Percentage")
        #         ))
        #       )
        #     )
        #   ),
        #   fluidRow(box(DT::dataTableOutput("table4"), width = 12)),
        #   downloadButton("download4", "Download Data")
        # ),
        # # Summary stats 2
        # tabItem(
        #   tabName = "Averages2",
        #   # h3("Averages"),
        #   fluidRow(
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab5Variable1",
        #         label = "Choose a time variable:",
        #         choices = c("Financial.Year")
        #       )
        #     ),
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab5Variable2",
        #         label = "Choose a grouping variable:",
        #         choices = names(MART.Dash %>% select(starts_with("Demographic")))
        #       )
        #     ),
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab5Variable3",
        #         label = "Choose a numeric variable:",
        #         choices = names(MART.Dash %>% select(
        #           `Surplus`, starts_with("Expenditure"), starts_with("Income"),
        #           starts_with("Percentage")
        #         ))
        #       )
        #     )
        #   ),
        #   fluidRow(
        #     column(
        #       width = 12,
        #       box(DT::dataTableOutput("table5"), width = 12)
        #     )
        #   ),
        #   fluidRow(
        #     column(
        #       width = 12,
        #       downloadButton("download5", "Download Data")
        #     )
        #   )
        # ),
        # # Modeling 1
        # tabItem(
        #   tabName = "Modeling",
        #   # h3("Modeling"),
        #   fluidRow(
        #     column(width = 6, sliderInput("number7a", "Water % Reduction:", min = 0, max = 100, value = 10)),
        #     column(width = 6, sliderInput("number7b", "Communications % Reduction:", min = 0, max = 100, value = 10))
        #   ),
        #   fluidRow(
        #     column(width = 6, sliderInput("number7c", "Car Insurance % Reduction:", min = 0, max = 100, value = 10)),
        #     column(width = 6, sliderInput("number7d", "Mortgage % Reduction:", min = 0, max = 100, value = 10))
        #   ),
        #   fluidRow(
        #     column(width = 6, sliderInput("number7e", "Transport % Reduction:", min = 0, max = 100, value = 10)),
        #     column(width = 6, sliderInput("number7f", "Food % Reduction:", min = 0, max = 100, value = 10))
        #   ),
        #   fluidRow(
        #     column(width = 6, sliderInput("number7g", "Energy % Reduction:", min = 0, max = 100, value = 10)),
        #     column(width = 6, sliderInput("number7h", "Leisure % Reduction:", min = 0, max = 100, value = 10))
        #   ),
        #   fluidRow(
        #     box(DT::dataTableOutput("table7"), width = 12)
        #   ),
        #   downloadButton("download7", "Download Data")
        # ),
        # # Modeling 2
        # tabItem(
        #   tabName = "Modeling2",
        #   # h3("Modeling"),
        #   fluidRow(
        #     column(width = 6, sliderInput("number8a", "Percentage Reduction:",
        #       min = 0, max = 100, value = 10
        #     )),
        #     column(width = 6, sliderInput("number8b", "Percentage Reduction:",
        #       min = 0, max = 100, value = 10
        #     ))
        #   ),
        #   fluidRow(
        #     column(width = 6, selectInput(
        #       inputId = "Tab8Variable1",
        #       label = "Choose an expenditure for the slider above:",
        #       choices = names(MART.Dash %>% select(starts_with("Expenditure")))
        #     )),
        #     column(width = 6, selectInput(
        #       inputId = "Tab8Variable2",
        #       label = "Choose an expenditure for the slider above:",
        #       choices = rev(names(MART.Dash %>% select(starts_with("Expenditure"))))
        #     ))
        #   ),
        #   fluidRow(box(DT::dataTableOutput("table8"), width = 12)),
        #   downloadButton("download8", "Download Data")
        # ),
        # # Modeling 3
        # tabItem(
        #   tabName = "Modeling3",
        #   # h3("Modeling"),
        #   fluidRow(
        #     column(
        #       width = 6,
        #       sliderInput("number9", "Percentage Increase:",
        #         min = 0, max = 100, value = 10
        #       )
        #     ),
        #     column(
        #       width = 6,
        #       selectInput(
        #         inputId = "Tab9Variable1",
        #         label = "Choose an income to increase:",
        #         choices = names(MART.Dash %>% select(starts_with("Income")))
        #       )
        #     )
        #   ),
        #   fluidRow(box(DT::dataTableOutput("table9"), width = 12)),
        #   downloadButton("download9", "Download Data")
        # ),
        tabItem(
          tabName = "Lines",
          # h3("Lines"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = "Tab10Variable1",
                label = "Choose a numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId = "TimeVariable",
                label = "Choose a time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot10"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table10"))
              )
            )
          ),
          downloadButton("download10", "Download Data"),
          downloadButton("download10b", "Download Graph")
        ),
        tabItem(
          tabName = "Lines2",
          # h3("Lines"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "Tab11Variable1",
                label = "Choose a grouping variable:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "Tab11Variable2",
                label = "Choose a numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "TimeVariable2",
                label = "Choose a time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot11"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table11"))
              )
            )
          ),
          downloadButton("download11", "Download Data"),
          downloadButton("download11b", "Download Graph")
        ),
        tabItem(
          tabName = "Lines3",
          # h3("Lines"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "Tab20Variable1",
                label = "Choose a grouping variable:",
                choices =
                  names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "TimeVariableNeg",
                label = "Choose a time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot20"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table20"))
              )
            )
          ),
          downloadButton("download20", "Download Data"),
          downloadButton("download20b", "Download Graph")
        ),
        # tabItem(
        #   tabName = "Lines4",
        #   # h3("Lines"),
        #   fluidRow(
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "Tab21Variable1",
        #         label = "Choose a grouping variable:",
        #         choices = names(MART.Dash %>% select(starts_with("Demographic")))
        #       )
        #     ),
        #     column(
        #       width = 4,
        #       selectInput(
        #         inputId = "TimeVariableNeg2",
        #         label = "Choose a time variable:",
        #         choices = c("Financial.Year")
        #       )
        #     )
        #   ),
        #   tabsetPanel(
        #     tabPanel(
        #       "Plot",
        #       fluidRow(
        #         column(width = 12, plotlyOutput("plot21"))
        #       )
        #     ),
        #     tabPanel(
        #       "Table",
        #       fluidRow(
        #         column(width = 12, DT::dataTableOutput("table21"))
        #       )
        #     )
        #   ),
        #   downloadButton("download21", "Download Data"),
        #   downloadButton("download21b", "Download Graph")
        # ),
        tabItem(
          tabName = "Bars",
          # h3("Bars"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "Tab12Variable1",
                label = "Choose a grouping variable:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "Tab12Variable2",
                label = "Choose a numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "TimeVariable3",
                label = "Choose a time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot12"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table12"))
              )
            )
          ),
          downloadButton("download12", "Download Data"),
          downloadButton("download12b", "Download Graph")
        ),
        tabItem(
          tabName = "Bars2",
          # h3("Bars"),
          fluidRow(
            column(
              width = 3,
              selectInput(
                inputId = "Tab13Variable1",
                label = "Grouping variable 1:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "Tab13Variable2",
                label = "Grouping variable 2:",
                choices = rev(names(MART.Dash %>% select(starts_with("Demographic"))))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "Tab13Variable3",
                label = "Numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "TimeVariable4",
                label = "Time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot13"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table13"))
              )
            )
          ),
          downloadButton("download13", "Download Data"),
          downloadButton("download13b", "Download Graph")
        ),
        tabItem(
          tabName = "Bars3",
          # h3("Bars"),
          fluidRow(
            column(
              width = 3,
              selectInput(
                inputId = "Tab14Variable1",
                label = "Grouping variable 1:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "Tab14Variable2",
                label = "Grouping variable 2:",
                choices = rev(names(MART.Dash %>% select(starts_with("Demographic"))))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "Tab14Variable3",
                label = "Numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "TimeVariable5",
                label = "Time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot14"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table14"))
              )
            )
          ),
          downloadButton("download14", "Download Data"),
          downloadButton("download14b", "Download Graph")
        ),
        tabItem(
          tabName = "Areas",
          # h3("Areas"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "Tab15Variable1",
                label = "Choose a grouping variable:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "Tab15Variable2",
                label = "Choose a numeric variable:",
                choices = names(MART.Dash %>% select(
                  `Surplus`, starts_with("Expenditure"), starts_with("Income"),
                  starts_with("Percentage")
                ))
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "TimeVariable6",
                label = "Choose a time variable:",
                choices = c("Financial.Year")
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot15"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table15"))
              )
            )
          ),
          downloadButton("download15", "Download Data"),
          downloadButton("download15b", "Download Graph")
        ),
        tabItem(
          tabName = "Areas2",
          # h3("Areas"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "Tab16Variable1",
                label = "Choose a grouping variable:",
                choices = names(MART.Dash %>% select(starts_with("Demographic")))
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              fluidRow(
                column(width = 12, plotlyOutput("plot16"))
              )
            ),
            tabPanel(
              "Table",
              fluidRow(
                column(width = 12, DT::dataTableOutput("table16"))
              )
            )
          ),
          downloadButton("download16", "Download Data"),
          downloadButton("download16b", "Download Graph"))
        # ),
        # tabItem(
        #   tabName = "Maps",
        #   # h3("Maps"),
        #   fluidRow(
        #     column(
        #       4,
        #       selectInput(
        #         inputId = "Boundary",
        #         label = "Select Boundary:",
        #         choices = c(
        #           "Region", "Local Authority", "BMRA",
        #           "Integrated.Care.Board"
        #         )
        #       )
        #     ),
        #     column(
        #       4,
        #       selectInput(
        #         inputId = "Tab18Variable1",
        #         label = "Choose a variable:",
        #         choices = names(MART.Dash %>% select(
        #           `Surplus`, starts_with("Expenditure"), starts_with("Income"),
        #           starts_with("Percentage")
        #         ))
        #       )
        #     ),
        #     column(
        #       4,
        #       selectInput(
        #         inputId = "Tab18Variable2",
        #         label = "Select a time period:",
        #         choices = c("Financial.Year")
        #       )
        #     ),
        #     column(
        #       4,
        #       selectInput(
        #         inputId = "Tab18Variable3",
        #         label = "Select a value:",
        #         choices = c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25")
        #       )
        #     ),
        #     column(
        #       4,
        #       selectInput(
        #         inputId = "Tab18Variable4",
        #         label = "Select a colour:",
        #         choices = c("Red", "Green")
        #       )
        #     )
        #   ),
        #   tabsetPanel(
        #     tabPanel(
        #       "Plot",
        #       fluidRow(
        #         column(width = 12, plotlyOutput("plot18"))
        #       )
        #     ),
        #     tabPanel(
        #       "Table",
        #       fluidRow(
        #         column(width = 12, DT::dataTableOutput("table18"))
        #       )
        #     )
        #   ),
        #   downloadButton("download18", "Download Data"),
        #   downloadButton("download18b", "Download Plot")
        # ),
        # tabItem(
        #   tabName = "Density",
        #   # h3("Density"),
        #   fluidRow(
        #     column(
        #       12,
        #       fluidRow(
        #         column(
        #           4,
        #           selectInput(
        #             inputId = "Tab17Variable1",
        #             label = "Choose a grouping variable:",
        #             choices = names(MART.Dash %>% select(starts_with("Demographic")))
        #           )
        #         ),
        #         column(
        #           4,
        #           selectInput(
        #             inputId = "Tab17Variable2",
        #             label = "Choose a numeric variable:",
        #             choices = names(MART.Dash %>% select(
        #               `Surplus`, starts_with("Expenditure"), starts_with("Income"),
        #               starts_with("Percentage")
        #             ))
        #           )
        #         ),
        #         column(
        #           4,
        #           selectInput(
        #             inputId = "Tab17Variable3",
        #             label = "Select a Year:",
        #             choices = c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25")
        #           )
        #         )
        #       ),
        #       plotlyOutput(outputId = "plot17")
        #     ),
        #     downloadButton("download17b", "Download Graph")
        #   ),
        # )
      )
    )
  )
} # Closing app_ui function
