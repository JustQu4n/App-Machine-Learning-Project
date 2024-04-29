library(shiny)
library(shinyWidgets)
library(caret)
library(ggplot2)
library(DT)
library(shinyalert)

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
                    tags$hr(),
                    
                    # Input: Select the type of feature scaling
                    radioButtons("scaling_type", "Choose Scaling Type:",
                                 choices = list("Standardization (Z-score)" = "standard",
                                                "Normalization (Min-Max)" = "normalize"),
                                 selected = "standard"),
                    
                    actionButton("scale_data", "Scale Data"),
                    
                    # Slider input for specifying the size of the training set
                    sliderInput("trainSize", "Percentage of the dataset to use as the training set:",
                                min = 0.1, max = 0.9, value = 0.7, step = 0.01),
                    
                    actionButton("split_data", "Split Data"),
                    
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
                             DT::dataTableOutput("processed_data"),
                             h4("Scale Feature"),
                             verbatimTextOutput("summaryScaled"),
                             h4("Splitting the dataset into Test set and Train set"),
                             verbatimTextOutput("summaryTrain"),
                             verbatimTextOutput("summaryTest")
                             )
                  ))
                ))