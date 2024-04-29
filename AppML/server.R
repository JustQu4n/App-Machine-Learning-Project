

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
