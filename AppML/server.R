

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
    if (is.null(inFile)){ return(NULL) }
    mydata <- read.csv(inFile$datapath, header = TRUE, sep=",")
    
  })
  

  output$xvariable <- renderUI({
    req(data())
    xa<-colnames(data()) 
    pickerInput(inputId = 'xvar',
                label = 'Select Input Variable',
                choices = c(xa[1:length(xa)]), selected=xa[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  
  output$yvariable <- renderUI({
    req(data())
    ya<-colnames(data()) 
    pickerInput(inputId = 'yvar',
                label = 'Select Target Variable',
                choices = c(ya[1:length(ya)]), selected=ya[2],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  # Dynamic UI for selecting multiple variables for missing value replacement
  output$var_select <- renderUI({
    req(data())
    colnames <- colnames(data())
    pickerInput("selected_vars", "Select Variables",
                choices = colnames,
                selected = colnames[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE) # Allow multiple selection
  })
  
  output$cat_vars_select <- renderUI({
    req(data())
    df <- data()
    cat_vars <- sapply(df, is.character) # Identify categorical variables
    if (any(cat_vars)) {
      pickerInput("selected_cat_vars", "Select Categorical Variables",
                  choices = names(df)[cat_vars],
                  options = list(`style` = "btn-info"),
                  multiple = TRUE)
    } else {
      "No categorical variables found."
    }
  })
  
  # Server logic to process data based on user input
  processedData <- eventReactive(input$process_data, {
    req(data())
    df <- data() # Get the data frame
    
    # Handle missing values as per user selection
    if (input$na_handling == "na.omit") {
      df <- na.omit(df)
    } else {
      targetVar <- input$selected_vars
      if (all(sapply(df[targetVar], is.numeric))) {
      naReplaceValue <- if (input$na_handling == "mean") {
        sapply(df[targetVar], function(x) mean(x, na.rm = TRUE))
      } else if (input$na_handling == "median") {
        sapply(df[targetVar], function(x) median(x, na.rm = TRUE))
      } else if (input$na_handling == "mode") {
        sapply(df[targetVar], function(x) {
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        })
      }
      
      for (var in targetVar) {
        df[[var]][is.na(df[[var]])] <- naReplaceValue[var]
      }
      }else{
        # Categorical variables: Replace with the most frequent value
        for (var in targetVar) {
          if (is.character(df[[var]])) {
            modeValue <- names(sort(table(df[[var]]), decreasing = TRUE))[1]
            df[[var]][is.na(df[[var]])] <- modeValue
          }
        }
      }
    }
    
    # Encode categorical variables as per user selection
    if (input$encoding_method == "onehot") {
      # One-Hot Encoding using model.matrix
      for (var in input$selected_cat_vars) {
        df <- cbind(df, model.matrix(~ get(var) - 1, data = df))
        df[[var]] <- NULL # Remove the original variable
      }
    } else if (input$encoding_method == "label") {
      # Label Encoding using forcats::fct_inorder
      for (var in input$selected_cat_vars) {
        df[[var]] <- as.integer(forcats::fct_inorder(df[[var]]))
      }
    }
    # Add other encoding methods here if needed
    
    df # Return the processed data frame
  
  })
  
  # Output: Display processed data
  output$processed_data <- renderDT({
    req(processedData())
    processedData()
  }, options = list(pageLength = 10))

  # Server logic to scale the data based on user input
  observeEvent(input$scale_data, {
    req(data())
    df <- data() # Get the data frame
    
    # Apply the chosen scaling method
    if (input$scaling_type == "standard") {
      # Standardization (Z-score)
      numeric_columns <- sapply(df, is.numeric)
      # Apply standardization only to numeric columns
      df[numeric_columns] <- scale(df[numeric_columns])
    } else if (input$scaling_type == "normalize") {
      # Normalization (Min-Max)
      min_max_normalize <- function(x) {
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      }
      df <- as.data.frame(lapply(df, min_max_normalize))
    }
    
    # Save the scaled data into a reactiveValues object
    values <- reactiveValues(scaledData = df)
    
    # Output: Display the summary of scaled data
    output$summaryScaled <- renderPrint({
      req(values$scaledData)
      summary(values$scaledData)
    })
  })
  
  # Server logic to split the data based on user input
  observeEvent(input$split_data, {
    req(data())
    df <- data() # Get the data frame
    
    # Calculate the size of the training set
    trainIndex <- floor(input$trainSize * nrow(df))
    
    # Split the data into training and test sets
    set.seed(123) # For reproducibility
    trainData <- df[1:trainIndex, ]
    testData <- df[(trainIndex + 1):nrow(df), ]
    
    # Save the split datasets into reactive values
    values <- reactiveValues(trainSet = trainData, testSet = testData)
    
    # Output: Display the summary of training and test sets
    output$summaryTrain <- renderPrint({
      req(values$trainSet)
      summary(values$trainSet)
    })
    
    output$summaryTest <- renderPrint({
      req(values$testSet)
      summary(values$testSet)
    })
  })
  output$tableDataset <-  DT::renderDataTable({
    data()
  })
  # Render the summary output
  output$summary <- renderPrint({
    req(data()) # Make sure the data is available
    summary(data()) # Display the summary of the dataset
  })
  # Output Data Info as a Table
  output$dataInfo <- renderTable({
    if (is.null(data())) {
      return(data.frame(Information = "Please upload a CSV file.", Value = NA))
    } else {
      num_rows <- nrow(data())
      num_cols <- ncol(data())
      num_duplicate_rows <- sum(duplicated(data()))
      num_rows_with_na <-
        sum(apply(data(), 1, function(x)
          any(is.na(x))))
      num_cols_with_na <-
        sum(apply(data(), 2, function(x)
          any(is.na(x))))
      
      info_df <- data.frame(
        Information = c(
          "Number of Rows",
          "Number of Columns",
          "Number of Duplicate Rows",
          "Number of Rows with Missing Values",
          "Number of Columns with Missing Values"
        ),
        Value = c(
          num_rows,
          num_cols,
          num_duplicate_rows,
          num_rows_with_na,
          num_cols_with_na
        )
      )
    }
  })
  output$colInfo <- renderTable({
    if (is.null(data())) {
      return(data.frame(Column = "No data available. Please upload a CSV file.", DataType = NA))
    } else {
      df <- data()  # Get the current data frame
      col_types <- sapply(df, class)  # Get data type of each column
      col_info_df <-
        data.frame(
          Column = names(df),
          DataType = col_types,
          stringsAsFactors = FALSE
        )
      return(col_info_df)
    }
  })
}
