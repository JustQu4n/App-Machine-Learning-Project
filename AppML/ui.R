library(shiny)
library(shinyWidgets)
library(caret)
library(ggplot2)
library(DT)

# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Simple Linear Regression"),
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput(
                      "file1",
                      "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    # Horizontal line ----
                    tags$hr(),
                    h4("Pre-Processing"),
                    # Input: Select method to handle missing values
                    selectInput(
                      "na_handling",
                      "Handle Missing Values",
                      choices = list(
                        "Exclude Rows" = "na.omit",
                        "Replace with Mean" = "mean",
                        "Replace with Median" = "median",
                        "Replace with Mode" = "mode"
                      ),
                    ),
                    # Input: Select variable for missing value replacement
                    uiOutput("var_select"),
                    
                    tags$hr(),
                    
                    # Input: Select categorical variables
                    uiOutput("cat_vars_select"),
                    
                    # Input: Select encoding method
                    selectInput("encoding_method", "Encoding Method",
                                choices = list("One-Hot Encoding" = "onehot",
                                               "Label Encoding" = "label"),
                                selected = "onehot"),
                    
                    actionButton("process_data", "Process Data"),
                    
                    # Horizontal line ----
                    tags$hr(),
                    uiOutput("yvariable"),
                    uiOutput("xvariable"),
                    
                    
                    # Input: Select models ----
                    selectInput(
                      "model_type",
                      "Model",
                      choices = list(
                        "Linear" = "linear",
                        "Logistic" = "logistic",
                        "KNN" = "knn",
                        "DecisionTree" = "decisiontree"
                      ),
                      selected = "linear"
                    ),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    actionButton("build_model", "Build Model")
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(tabsetPanel(
                    tabPanel(
                      "Data Information",
                      fluidRow(column(6,
                                      tableOutput("dataInfo")),
                               column(6,
                                      tableOutput("colInfo"))),
                      verbatimTextOutput("summary"),
                      DT::dataTableOutput("tableDataset")
                      
                    ),
                    tabPanel("Data Preprocessing",
                             h4("Datatable after Missing Value"),
                             DT::dataTableOutput("processed_data"),
                             h4("Datatable after Encoder"),
                             DT::dataTableOutput("encoded_data"),
                             )
                  ))
                ))