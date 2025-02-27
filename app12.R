library(shiny)
library(rmarkdown)
library(shinyjs)
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
     .modal-dialog {
        width: 80%; /* Set the width of the modal dialog */
        max-width: 80%; /* Ensure it doesn't exceed 80% of the screen */
        height: 80%; /* Set the height of the modal dialog */
        max-height: 80%; /* Ensure it doesn't exceed 80% of the screen */
      }
      .modal-content {
        height: 100%; /* Make sure the content covers the entire modal */
        overflow-y: auto; /* Enable scrolling if the content is too large */
      }
      .modal-header {
        background-color: #EBD3F8;
        color: white;
      }
      .modal-header {
      
        background-color: #EBD3F8;
        color: white;
      }
       body {
        background-color: #EBD3F8; 
        color: #2E073F; 
       }
      #main_title {
        text-align: center;
        font-size: 40px;
       
        font-weight: bold;
        margin-bottom: 20px;
      }
      #description {
        text-align: center;
        font-size: 18px;
        margin-bottom: 30px;
        background-color:  #EDF4F2; 
        color: #704264; 
        border-radius: 15px;
        padding: 15px; /* Padding inside the message box */
        width: 60%; /* Width of the message box */
        margin: 0 auto; /* Center the message box */
        position: relative; /* Relative positioning for the speech bubble */
      }
      #description:before {
        content: '';
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -10px;
        border-width: 10px;
        border-style: solid;
        border-color: #9370DB transparent transparent transparent;
      }
      .modal-footer {
        background-color: #f8f9fa;
      }
      .btn-custom {
        background-color: #EBD3F8;
        color:#2E073F;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-custom-green {
        background-color:#EBD3F8;
        color: #2E073F;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-custom-orange {
        background-color: #EBD3F8;
        color: #2E073F;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-custom-red {
        background-color: #EBD3F8;
        color: #2E073F;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-custom-blue {
        background-color: #EBD3F8;
        color: #2E073F;
        width: 100%;
        margin-bottom: 10px;
      }
    "))
  ),
  div(id = "splash",
      style = "position: absolute; width: 100%; height: 100%; background-color: #gyhff; display: flex; flex-direction: column; justify-content: center; align-items: center; z-index: 9999;",
      h1("DATA ANALYSIS VIA INTERACTIVE DESIGN", style = "font-size: 5em; color: #2E073F;"),
      div(id = "description","Data Analysis Via Interactive Design"),
      h4("Developed by : Aiswarya P Mani, Dr. David CR, Dr. Dipu S")
  ),
  hidden(
    div(id = "main_ui",
        div(id = "main_title", "DATA ANALYSIS VIA INTERACTIVE DESIGN"), # Centered title
        div(id = "description", "Your Comprehensive Tool for Statistical Analysis"),
  verticalLayout(
    actionButton("btn_hypotheses", "Hypotheses Checking", icon = icon("search-plus"), style = "background-color: #7A1CAC; color: white; width:100%; margin-bottom: 10px;"),
    actionButton("btn_regression", "Regression", icon = icon("chart-line"), style = "background-color: #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
    actionButton("btn_eda", "EDA", icon = icon("chart-bar"), style = "background-color: #E49BFF; color: white; width:100%; margin-bottom: 10px;")
  )
)
)
)

server <- function(input, output, session) {
  delay(1000, {
    shinyjs::hide("splash")
    shinyjs::show("main_ui")
  })
  data <- reactiveValues(file = NULL)
  t_state <- reactiveVal()
  
  observeEvent(input$btn_hypotheses, {
    showModal(modalDialog(
      title = "Hypotheses Checking",
      div(id = "description", "To conduct hypothesis testing, a dataset is required. Kindly provide the dataset in .csv file format, including all necessary variables and data."),
      fileInput("file_hypotheses", "Upload your dataset (.csv file format):"),
      actionButton("next_hypotheses", "NEXT", icon = icon("arrow-right"), style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  
  observeEvent(input$file_hypotheses, {
    data$file <- read.csv(input$file_hypotheses$datapath)
  })
  
  observeEvent(input$next_hypotheses, {
    removeModal()
    showModal(modalDialog(
      title = "Test Type",
      div(id = "description", "Please select one sample test if you want to check whether a single group differs from a known value , please  select two  sample test if you want to evaluate  whether two groups differ from each other."),
      actionButton("one_sample_test", "One Sample Test", style = "background-color: #7A1CAC; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("two_sample_test", "Two Sample Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;")
    ))
  })
  observeEvent(input$two_sample_test, {
    removeModal()
    showModal(modalDialog(
      title = "Test Type",
      div(id = "description", "Please select one sample test if you want to check whether a single group differs from a known value , please  select two  sample test if you want to evaluate  whether two groups differ from each other."),
      actionButton("parametric", "Parametric Test", style = "background-color: #7A1CAC; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("non_parametric", "Non Parametric Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;")
    ))
  })
  
  observeEvent(input$one_sample_test, {
    removeModal()
    showModal(modalDialog(
      title = "One Sample Test",
      div(id="description","Please select the test you wish to conduct, Two-Sample Test for single group comparison, "),
      actionButton("z_test", "Z-Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("chi_square_test", "Chi-Square Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("sign_test", "Sign Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;")
    ))
  })
  
  observeEvent(input$parametric, {
    removeModal()
    showModal(modalDialog(
      title = "Two Sample Test",
      div(id="description","Please select the test you wish to conduct, One-Sample Test for single group comparison, "),
      actionButton("twosample_t_test", "Two-Sample T-Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("twosample_Paired_test", "Paired T-Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("twosample_z_test", "Z-Test for Proportions", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("sign_test", "Sign Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("Two_sample_f_test", "F-Test", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;")
    ))
  }) 
  
  
  
  
  


  observeEvent(input$z_test, {
    removeModal()
    showModal(modalDialog(
      title = "Select Column for z-Test",
      div(id="description","One key assumption of the z-test is that the data should be approximately normally distributed. To verify this, first select the appropriate column from your dataset to perform statistical tests, such as the Shapiro-Wilk test, to assess normality"),
      selectInput("select_column", "Select the field", choices = names(data$file)),
      actionButton("check_normality", "Check Normality", icon = icon("check-circle"), style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  observeEvent(input$check_normality, {
    selected_column <- data$file[[input$select_column]]
    shapiro_test <- shapiro.test(selected_column)
    
    
    print(paste("Shapiro test p-value:", shapiro_test$p.value))
    
    normality_check <- (shapiro_test$p.value > 0.05)
    
    print(paste("Normality check result:", normality_check))
    
    removeModal()
    
    if (normality_check) {
      showModal(modalDialog(
        title = "Normality Test Result",
        div(id="discription","The data is approximately normally distributed, as indicated by the Shapiro-Wilk test (p-value > 0.05). Therefore, it is appropriate to proceed with the Z-test"),
        p("The data follows a normal distribution (p-value > 0.05)."),
        actionButton("next_normality", "NEXT", style = "background-color: #EBD3F8; color:#2E073F; width:100%;")
      ))
    } else {
      showModal(modalDialog(
        title = "Normality Test Result",
        div(id="discription","The data does not follow a normal distribution, as indicated by the Shapiro-Wilk test (p-value ≤ 0.05). Therefore, it is recommended to use a non-parametric test instead of the t-test."),
        p("The data does not follow a normal distribution (p-value <= 0.05). You need to perform a non-parametric test."),
        actionButton("close_normality", "CLOSE", style = "background-color: #EBD3F8; color:#2E073F; width:100%;")
      ))
    }
  })
  
  
  # Gather Z-Test parameters
  observeEvent(input$next_normality, {
    removeModal()
    showModal(modalDialog(
      div(id="description", "Please provide the population mean  for the Z-test."),
      title = "Z-Test Parameters",
      textInput("population_mean", "Population Mean:", value = ""),
      
      
      actionButton("next_mean", "Nextmean", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  observeEvent(input$next_mean, {
    removeModal()
    showModal(modalDialog(
      div(id="description", "Please provide the population population standard deviation and Significant level alpha for the Z-test."),
      title = "Z-Test Parameters",
      
      textInput("population_sd", "Population Standard Deviation:", value = ""),
      textInput("significant_level", "Significant Level (alpha):", value = ""),
      actionButton("next_sd", "Nextsd", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  
  
  observeEvent(input$close_normality, {
    removeModal()
  })
  
  observeEvent(input$next_sd, {
    removeModal()
    showModal(modalDialog(
      title = "Choose the Alternative Hypotheses",
      div(id="description", "Please select the alternative hypothesis for conducting this test to determine the direction of the statistical analysis."),
      actionButton("lessthan", "M < M0", style = "background-color: #7A1CAC; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("greaterthan", "M > M0", style = "background-color:  #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("equalto", "M != M0", style = "background-color: #E49BFF; color: white; width:100%; margin-bottom: 10px;")
    ))
  })
  
  observeEvent(input$lessthan, {
    perform_z_test("less")
    z_state("lessthan")
  })
  
  observeEvent(input$greaterthan, {
    perform_z_test("greater")
    z_state("greaterthan")
  })
  
  observeEvent(input$equalto, {
    perform_z_test("two.sided")
    z_state("equalto")
  })
  z_state <- reactiveVal()
  
  perform_z_test <- function(alternative) {
    removeModal()
    showModal(modalDialog(
      title = "Z-Test Results",
      verbatimTextOutput("z_test_result"),
      verbatimTextOutput("conclusion"),
      downloadButton("download_report", "Download Report", style = "background-color: #2E073F; color: white; width:100%; margin-top: 10px;")
    ))
    
    output$z_test_result <- renderPrint({
      selected_column <- data$file[[input$select_column]]
      sample_mean <- mean(selected_column)
      population_mean <- as.numeric(input$population_mean)
      population_sd <- as.numeric(input$population_sd)
      sample_size <- length(selected_column)
      significance_level <- as.numeric(input$significant_level)
      
      # Calculate z-statistic
      z_statistic <- (sample_mean - population_mean) / (population_sd / sqrt(sample_size))
      
      # Display results
      paste("Z-Statistic:", z_statistic, "\n",
            "Sample Mean:", sample_mean, "\n",
            "Sample Size:", sample_size, "\n",
            "Population Standard Deviation:", population_sd)
    })
    
    output$conclusion <- renderPrint({
      selected_column <- data$file[[input$select_column]]
      sample_mean <- mean(selected_column)
      population_mean <- as.numeric(input$population_mean)
      population_sd <- as.numeric(input$population_sd)
      sample_size <- length(selected_column)
      z_statistic <- (sample_mean - population_mean) / (population_sd / sqrt(sample_size))
      significance_level <- as.numeric(input$significant_level)
      
      # Determine critical values and conclusion
      if (alternative == "less") {
        critical_value <- qnorm(significance_level, lower.tail = TRUE)
        conclusion <- if (z_statistic < critical_value) {
          "Reject the null hypothesis: Mean is less than the hypothesized mean."
        } else {
          "Fail to reject the null hypothesis."
        }
      } else if (alternative == "greater") {
        critical_value <- qnorm(1 - significance_level, lower.tail = TRUE)
        conclusion <- if (z_statistic > critical_value) {
          "Reject the null hypothesis: Mean is greater than the hypothesized mean."
        } else {
          "Fail to reject the null hypothesis."
        }
      } else if (alternative == "two.sided") {
        critical_value <- qnorm(1 - significance_level / 2)
        conclusion <- if (abs(z_statistic) > critical_value) {
          "Reject the null hypothesis: Mean is different from the hypothesized mean."
        } else {
          "Fail to reject the null hypothesis."
        }
      }
      paste("Conclusion based on t-statistic:", conclusion)
    })
    
    
    output$download_report <- downloadHandler(
      filename = function() {
        "Z-Test_Report.docx"
      },
      content = function(file) {
        # Path to the R Markdown file
        rmd_path <- "zTest_Report.Rmd"  # Ensure this file is in the working directory
        tempReport <- file.path(tempdir(), "zTest_Report.Rmd")
        
        # Copy the R Markdown file to the temporary directory
        file.copy(rmd_path, tempReport, overwrite = TRUE)
        
        # Perform Z-test calculations
        selected_column <- data$file[[input$select_column]]
        sample_mean <- mean(selected_column)
        population_mean <- as.numeric(input$population_mean)
        population_sd <- as.numeric(input$population_sd)
        sample_size <- length(selected_column)
        significance_level <- as.numeric(input$significant_level)
        z_statistic <- (sample_mean - population_mean) / (population_sd / sqrt(sample_size))
        
        # Determine critical values and conclusion
        conclusion <- if (z_state() == "lessthan") {
          critical_value <- qnorm(significance_level, lower.tail = TRUE)
          if (z_statistic < critical_value) {
            "Reject the null hypothesis: Mean is less than the hypothesized mean."
          } else {
            "Fail to reject the null hypothesis."
          }
        } else if (z_state() == "greaterthan") {
          critical_value <- qnorm(1 - significance_level, lower.tail = TRUE)
          if (z_statistic > critical_value) {
            "Reject the null hypothesis: Mean is greater than the hypothesized mean."
          } else {
            "Fail to reject the null hypothesis."
          }
        } else if (z_state() == "equalto") {
          critical_value <- qnorm(1 - significance_level / 2)
          if (abs(z_statistic) > critical_value) {
            "Reject the null hypothesis: Mean is different from the hypothesized mean."
          } else {
            "Fail to reject the null hypothesis."
          }
        }
        
        # Define parameters for the R Markdown document
        params <- list(
          z_state = z_state(),
          sample_mean = sample_mean,
          sample_size = sample_size,
          population_mean = population_mean,
          population_sd = population_sd,
          z_statistic = z_statistic,
          significance_level = significance_level,
          conclusion_z_statistic = conclusion
        )
        
        # Render the R Markdown file to a Word document
        rmarkdown::render(tempReport, output_format = "word_document", output_file = file, params = params, envir = new.env())
      }
    )
    
  }
  observeEvent(input$sign_test, {
    removeModal()
    showModal(modalDialog(
      title = "Select Column for Sign Test",
      div(id = "description", "The Sign Test is a non-parametric test used to determine if the median of a sample differs significantly from a specified value. Please select the appropriate column for analysis."),
      selectInput("select_column_sign", "Select the field", choices = names(data$file)),
      actionButton("next_step_sign", "NEXT", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  observeEvent(input$next_step_sign, {
    removeModal()
    showModal(modalDialog(
      title = "Enter Hypothesized Median",
      div(id = "description", "Please enter the hypothesized median value for the sign test."),
      tags$div(textInput("median_value", "Enter Hypothesized Median:", value = ""), style = "width:100%"),
      actionButton("next_hypothesis", "NEXT", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  observeEvent(input$next_hypothesis, {
    removeModal()
    showModal(modalDialog(
      title = "Select Alternative Hypothesis",
      div(id = "description", "Please select the alternative hypothesis for conducting this test."),
      radioButtons("alternative_hypothesis", "Select Alternative Hypothesis:",
                   choices = list(
                     "Median < Hypothesized Median" = "less",
                     "Median > Hypothesized Median" = "greater",
                     "Median != Hypothesized Median" = "two.sided"
                   ),
                   selected = "two.sided"
      ),
      actionButton("run_sign_test", "Run Sign Test", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  
  observeEvent(input$run_sign_test, {
    removeModal()
    
    # Extract selected data and user inputs
    selected_column <- data$file[[input$select_column_sign]]
    hypothesized_median <- as.numeric(input$median_value)
    alternative <- input$alternative_hypothesis
    
    # Perform Sign Test
    library(BSDA)  # Ensure BSDA package is installed
    sign_test_result <- SIGN.test(selected_column, md = hypothesized_median, alternative = alternative)
    
    # Extract test results
    test_stat <- as.numeric(sign_test_result$statistic)  # Number of positive differences (S)
    p_value <- sign_test_result$p.value
    conf_int <- sign_test_result$conf.int
    sample_size <- length(selected_column)  # Total number of observations
    
    # Determine critical value (approximate, based on binomial properties)
    critical_value <- qbinom(0.05, sample_size, 0.5)  # Critical S value for α = 0.05
    
    # Generate Conclusion Based on Test Statistic and P-value
    if (alternative == "less") {
      if (p_value < 0.05 && test_stat < critical_value) {
        conclusion <- "Reject the null hypothesis: The sample median is significantly less than the hypothesized median at a 5% significance level."
      } else {
        conclusion <- "Fail to reject the null hypothesis: The sample median is not significantly less than the hypothesized median."
      }
    } else if (alternative == "greater") {
      if (p_value < 0.05 && test_stat > (sample_size - critical_value)) {
        conclusion <- "Reject the null hypothesis: The sample median is significantly greater than the hypothesized median at a 5% significance level."
      } else {
        conclusion <- "Fail to reject the null hypothesis: The sample median is not significantly greater than the hypothesized median."
      }
    } else if (alternative == "two.sided") {
      if (p_value < 0.05 && (test_stat < critical_value || test_stat > (sample_size - critical_value))) {
        conclusion <- "Reject the null hypothesis: The sample median is significantly different from the hypothesized median at a 5% significance level."
      } else {
        conclusion <- "Fail to reject the null hypothesis: The sample median does not significantly differ from the hypothesized median."
      }
    }
    
    # Show results in a modal
    showModal(modalDialog(
      title = "Sign Test Results",
      verbatimTextOutput("sign_test_results"),
      verbatimTextOutput("sign_test_conclusion"),
      downloadButton("download_sign_test_report", "Download Report", style = "background-color: #2E073F; color: white; width:100%; margin-top: 10px;")
    ))
    
    # Render outputs
    output$sign_test_results <- renderPrint({
      paste(
        "Test Statistic (S):", test_stat, "\n",
        "Sample Size (n):", sample_size, "\n",
        "P-value:", p_value, "\n",
        "Critical S Value (α = 0.05):", critical_value, "\n",
        "Confidence Interval:", paste(conf_int, collapse = " to ")
      )
    })
    
    output$sign_test_conclusion <- renderPrint({ conclusion })
    
    # Report generation
    output$download_sign_test_report <- downloadHandler(
      filename = function() { "Sign_Test_Report.docx" },
      content = function(file) {
        tempReport <- file.path(tempdir(), "sign_test_report.Rmd")
        file.copy("sign_test_report.Rmd", tempReport, overwrite = TRUE)
        
        # Prepare parameters for R Markdown
        params <- list(
          selected_column = input$select_column_sign,
          hypothesized_median = hypothesized_median,
          alternative = alternative,
          test_stat = test_stat,
          sample_size = sample_size,
          p_value = p_value,
          critical_value = critical_value,
          conf_int = conf_int,
          conclusion = conclusion
        )
        
        rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env())
      }
    )
  })
  
  
  observeEvent(input$chi_square_test, {
    removeModal()
    showModal(modalDialog(
      title = "Select Column for Chi-Square Test",
      div(id = "description", "To perform the one-sample chi-square test for variance, first select a numeric column from your dataset."),
      selectInput("select_column_chi", "Select the Field:", choices = names(data$file)),
      actionButton("next_select_variance", "NEXT", icon = icon("arrow-right"), style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  # Step 2: Collect hypothesized population variance
  observeEvent(input$next_select_variance, {
    removeModal()
    showModal(modalDialog(
      title = "Chi-Square Test Parameters",
      div(id = "description", "Please provide the hypothesized population variance and significance level for the test."),
      numericInput("pop_variance", "Population Variance:", value = 1, min = 0),
      numericInput("significance_level_chi", "Significance Level (alpha):", value = 0.05, min = 0, max = 1),
      actionButton("next_variance_params", "NEXT", style = "background-color: #EBD3F8; color: #2E073F; width:100%;")
    ))
  })
  
  # Step 3: Choose alternative hypothesis
  observeEvent(input$next_variance_params, {
    removeModal()
    showModal(modalDialog(
      title = "Choose the Alternative Hypothesis",
      div(id = "description", "Please select the alternative hypothesis for conducting this test to determine the direction of the statistical analysis."),
      actionButton("less_var", "σ² < σ₀²", style = "background-color: #7A1CAC; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("greater_var", "σ² > σ₀²", style = "background-color: #AD49E1; color: white; width:100%; margin-bottom: 10px;"),
      actionButton("two_sided_var", "σ² ≠ σ₀²", style = "background-color: #E49BFF; color: white; width:100%; margin-bottom: 10px;")
    ))
  })
  
  # Step 4: Perform Chi-Square Test
  observeEvent(input$less_var, {
    perform_chi_square_test("less")
    chi_state("less")
  })
  observeEvent(input$greater_var, {
    perform_chi_square_test("greater")
    chi_state("greater")
  })
  observeEvent(input$two_sided_var, {
    perform_chi_square_test("two.sided")
    chi_state("two.sided")
  })
  
  chi_state <- reactiveVal()
  
  perform_chi_square_test <- function(alternative) {
    removeModal()
    showModal(modalDialog(
      title = "Chi-Square Test Results",
      verbatimTextOutput("chi_sq_result"),
      verbatimTextOutput("chi_conclusion"),
      downloadButton("download_chi_report", "Download Report", style = "background-color: #2E073F; color: white; width:100%; margin-top: 10px;")
    ))
    
    # Reactive to store conclusion
    conclusion <- reactiveVal()
    
    # Compute the Chi-Square Test
    output$chi_sq_result <- renderPrint({
      selected_column <- data$file[[input$select_column_chi]]
      sample_variance <- var(selected_column, na.rm = TRUE)
      sample_size <- length(na.omit(selected_column))
      pop_variance <- as.numeric(input$pop_variance)
      significance_level <- as.numeric(input$significance_level_chi)
      
      # Calculate test statistic
      chi_sq_stat <- (sample_size - 1) * sample_variance / pop_variance
      df <- sample_size - 1
      
      # Display results
      paste("Chi-Square Statistic:", round(chi_sq_stat, 4), "\n",
            "Sample Variance:", round(sample_variance, 4), "\n",
            "Degrees of Freedom:", df)
    })
    
    output$chi_conclusion <- renderPrint({
      selected_column <- data$file[[input$select_column_chi]]
      sample_variance <- var(selected_column, na.rm = TRUE)
      sample_size <- length(na.omit(selected_column))
      pop_variance <- as.numeric(input$pop_variance)
      chi_sq_stat <- (sample_size - 1) * sample_variance / pop_variance
      df <- sample_size - 1
      significance_level <- as.numeric(input$significance_level_chi)
      
      # Determine critical values and conclusion
      if (alternative == "less") {
        critical_value <- qchisq(significance_level, df, lower.tail = TRUE)
        conclusion_text <- if (chi_sq_stat < critical_value) {
          "Reject the null hypothesis: The variance is less than the hypothesized population variance."
        } else {
          "Fail to reject the null hypothesis."
        }
      } else if (alternative == "greater") {
        critical_value <- qchisq(1 - significance_level, df, lower.tail = TRUE)
        conclusion_text <- if (chi_sq_stat > critical_value) {
          "Reject the null hypothesis: The variance is greater than the hypothesized population variance."
        } else {
          "Fail to reject the null hypothesis."
        }
      } else if (alternative == "two.sided") {
        lower_critical <- qchisq(significance_level / 2, df, lower.tail = TRUE)
        upper_critical <- qchisq(1 - significance_level / 2, df, lower.tail = TRUE)
        conclusion_text <- if (chi_sq_stat < lower_critical || chi_sq_stat > upper_critical) {
          "Reject the null hypothesis: The variance differs from the hypothesized population variance."
        } else {
          "Fail to reject the null hypothesis."
        }
      }
      
      # Store conclusion in reactiveVal
      conclusion(conclusion_text)
      paste("Conclusion:", conclusion_text)
    })
    
    # Generate Report
    output$download_chi_report <- downloadHandler(
      filename = function() {
        "Chi-Square_Test_Report.docx"
      },
      content = function(file) {
        rmd_path <- "chi_square_report.Rmd"  # Ensure this file exists
        tempReport <- file.path(tempdir(), "chi_square_report.Rmd")
        file.copy(rmd_path, tempReport, overwrite = TRUE)
        
        selected_column <- data$file[[input$select_column_chi]]
        sample_variance <- var(selected_column, na.rm = TRUE)
        sample_size <- length(na.omit(selected_column))
        pop_variance <- as.numeric(input$pop_variance)
        chi_sq_stat <- (sample_size - 1) * sample_variance / pop_variance
        
        params <- list(
          chi_state = chi_state(),
          sample_variance = sample_variance,
          sample_size = sample_size,
          pop_variance = pop_variance,
          chi_sq_stat = chi_sq_stat,
          significance_level = as.numeric(input$significance_level_chi),
          conclusion = conclusion()
        )
        
        rmarkdown::render(tempReport, output_format = "word_document", output_file = file, params = params, envir = new.env())
      }
    )
  }
}
shinyApp(ui, server)

