# Load packages
library(shiny)
library(GDINA)
library(shinythemes)
library(DT)
library(ggplot2)
library(fmsb)  # For radar chart

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("GDINA Analysis (Generalized Deterministic Input, Noisy And Gate)"),
  
  # Contact information in a well panel at the top
  wellPanel(
    style = "background-color: #f8f9fa; border-color: #17a2b8;",
    fluidRow(
      column(12, 
             div(style = "text-align: center;",
                 h4("Contact Information"),
                 tags$p(
                   tags$i(class = "fa fa-whatsapp", style = "color: #25D366;"), 
                   " WhatsApp: ",
                   tags$a(href = "https://wa.me/6288103840816", target = "_blank", "+62 881-0384-00816 (Indhi Wiradika)")
                 ),
                 tags$p(
                   tags$i(class = "fa fa-instagram", style = "color: #E1306C;"), 
                   " Instagram: ",
                   tags$a(href = "https://www.instagram.com/assevlabs/", target = "_blank", "@assevlabs")
                 )
             )
      )
    )
  ),
  
  # Include Font Awesome for social media icons
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("respFile", "Upload Response File:",
                accept = c(".txt", ".csv", ".dat")),
      fileInput("qMatrix", "Upload Q-Matrix File:",
                accept = c(".txt", ".csv", ".dat")),
      
      hr(),
      
      selectInput("modelType", "Select Model:",
                  choices = c("GDINA", "DINA", "DINO", "ACDM", "LLM"),
                  selected = "GDINA"),
      
      numericInput("maxitr", "Maximum Iterations:", 
                   value = 100, min = 10, max = 1000),
      
      selectInput("convMethod", "Convergence Method:",
                  choices = c("deviance", "pattern", "parameter"),
                  selected = "deviance"),
      
      actionButton("runModel", "Run Model", 
                   class = "btn-primary"),
      
      hr(),
      
      conditionalPanel(
        condition = "output.modelReady",
        downloadButton("downloadResults", "Download Results", 
                       class = "btn-success"),
        downloadButton("downloadRadarChart", "Download Radar Chart", 
                       class = "btn-info mt-2")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Data", 
                 h4("Response Data Preview"),
                 DTOutput("respPreview"),
                 h4("Q-Matrix Preview"),
                 DTOutput("qMatrixPreview")
        ),
        
        tabPanel("Model Results", 
                 conditionalPanel(
                   condition = "output.modelReady",
                   h4("Model Summary"),
                   verbatimTextOutput("modelSummary"),
                   h4("Item Parameters"),
                   DTOutput("itemParam")
                 ),
                 conditionalPanel(
                   condition = "!output.modelReady",
                   h4("Please upload data and run the model first.")
                 )
        ),
        
        tabPanel("Diagnostics", 
                 conditionalPanel(
                   condition = "output.modelReady",
                   h4("Attribute Probability"),
                   fluidRow(
                     column(6, plotOutput("attrProbPlot")),
                     column(6, plotOutput("radarChart"))
                   ),
                   h4("Item Fit"),
                   DTOutput("itemFit")
                 ),
                 conditionalPanel(
                   condition = "!output.modelReady",
                   h4("Please upload data and run the model first.")
                 )
        ),
        
        tabPanel("Classification", 
                 conditionalPanel(
                   condition = "output.modelReady",
                   h4("Attribute Classification for Each Individual"),
                   DTOutput("attrPattern"),
                   h4("Attribute Pattern Distribution"),
                   plotOutput("attrPatternPlot")
                 ),
                 conditionalPanel(
                   condition = "!output.modelReady",
                   h4("Please upload data and run the model first.")
                 )
        ),
        
        tabPanel("Individual Profiles", 
                 conditionalPanel(
                   condition = "output.modelReady",
                   fluidRow(
                     column(4,
                            selectInput("studentSelect", "Select Individual:", choices = NULL),
                            plotOutput("individualRadarChart", height = "400px"),
                            h4("Individual Attribute Mastery"),
                            verbatimTextOutput("individualMastery")
                     ),
                     column(8,
                            h4("Cognitive Profile Summary"),
                            verbatimTextOutput("profileSummary"),
                            h4("Attribute Mastery Pattern"),
                            DTOutput("individualPattern")
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "!output.modelReady",
                   h4("Please upload data and run the model first.")
                 )
        ),
        
        tabPanel("Help", 
                 h3("User Guide"),
                 tags$ul(
                   tags$li("Upload test response file (.csv, .txt, .dat). Format: Rows = individuals, Columns = items, Values = responses (0/1)"),
                   tags$li("Upload Q-Matrix file (.csv, .txt, .dat). Format: Rows = items, Columns = attributes, Values = item-attribute relationships (0/1)"),
                   tags$li("Select the CDM model to run"),
                   tags$li("Adjust other parameters as needed"),
                   tags$li("Click 'Run Model' to start the analysis"),
                   tags$li("View analysis results in the available tabs")
                 ),
                 
                 h3("Model Information"),
                 tags$ul(
                   tags$li(tags$b("GDINA:"), "General model allowing flexible relationships between attributes"),
                   tags$li(tags$b("DINA:"), "Model with conjunctive assumption (all attributes required)"),
                   tags$li(tags$b("DINO:"), "Model with disjunctive assumption (any one attribute sufficient)"),
                   tags$li(tags$b("ACDM:"), "Additive model considering partial contribution from attributes"),
                   tags$li(tags$b("LLM:"), "Log-linear model accommodating interactions between attributes")
                 ),
                 
                 h3("About Cognitive Diagnosis Models"),
                 p("Cognitive Diagnosis Models (CDMs) are psychometric tools designed to diagnose specific cognitive strengths and 
                    weaknesses based on assessment responses. These models are particularly useful in educational contexts for identifying
                    which specific skills or knowledge components (attributes) students have mastered or not mastered."),
                 p("The GDINA (Generalized Deterministic Inputs, Noisy \"And\" Gate) is a general CDM framework that subsumes many specific
                    models such as DINA, DINO, ACDM, and LLM. This interface provides access to these models through the GDINA R package.")
        ),
        
        tabPanel("About", 
                 h3("About This Application"),
                 p("This application was developed to facilitate Cognitive Diagnostic Analysis using the GDINA framework. 
                    It provides a user-friendly interface for educational researchers, psychometricians, and assessment specialists
                    to analyze student response data and identify mastery patterns of cognitive attributes."),
                 
                 h3("Developer Information"),
                 p(tags$b("Developer:"), "Indhi Wiradika"),
                 p(tags$b("Organization:"), "AssevLabs"),
                 p(tags$b("Contact Information:")),
                 tags$ul(
                   tags$li(tags$i(class = "fa fa-whatsapp", style = "color: #25D366;"), 
                           " WhatsApp: ",
                           tags$a(href = "https://wa.me/6288103840816", target = "_blank", "+62 881-0384-00816")),
                   tags$li(tags$i(class = "fa fa-instagram", style = "color: #E1306C;"), 
                           " Instagram: ",
                           tags$a(href = "https://www.instagram.com/assevlabs/", target = "_blank", "@assevlabs"))
                 ),
                 
                 h3("Citation"),
                 p("If you use this application in your research, please cite:"),
                 tags$blockquote(
                   "Wiradika, I.N.I. (2025). GDINA Analysis GUI: A tool for cognitive diagnostic assessment. AssevLabs."
                 ),
                 
                 h3("Acknowledgments"),
                 p("This application uses the GDINA R package developed by Wenchao Ma, Jimmy de la Torre, and Miguel Sorrel.")
        )
      )
    )
  ),
  
  # Footer with additional contact info
  tags$footer(
    div(
      class = "footer",
      style = "text-align: center; padding: 10px; margin-top: 20px; border-top: 1px solid #ddd;",
      p("© 2025 AssevLabs. All rights reserved."),
      p("For support, please contact Indhi Wiradika at ",
        tags$a(href = "https://wa.me/6288103840816", target = "_blank", "+62 881-0384-00816"), 
        " or visit ",
        tags$a(href = "https://www.instagram.com/assevlabs/", target = "_blank", "@assevlabs"), 
        " on Instagram."
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    respData = NULL,
    qMatrix = NULL,
    modelResult = NULL,
    modelReady = FALSE,
    attributeNames = NULL
  )
  
  # Read response data
  observeEvent(input$respFile, {
    inFile <- input$respFile
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    if (ext == "csv") {
      values$respData <- read.csv(inFile$datapath, header = FALSE)
    } else {
      values$respData <- as.data.frame(read.table(inFile$datapath, header = FALSE))
    }
  })
  
  # Read Q-matrix
  observeEvent(input$qMatrix, {
    inFile <- input$qMatrix
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    if (ext == "csv") {
      values$qMatrix <- read.csv(inFile$datapath, header = FALSE)
    } else {
      values$qMatrix <- as.data.frame(read.table(inFile$datapath, header = FALSE))
    }
    
    # Create attribute names (either from column names or default)
    num_attrs <- ncol(values$qMatrix)
    if (!is.null(colnames(values$qMatrix)) && sum(colnames(values$qMatrix) != "") == num_attrs) {
      values$attributeNames <- colnames(values$qMatrix)
    } else {
      # Default computational thinking attribute names if available
      if (num_attrs == 5) {
        values$attributeNames <- c("Decomposition", "Abstraction", "Algorithmic", "Pattern", "Automation")
      } else {
        values$attributeNames <- paste("Attribute", 1:num_attrs)
      }
    }
  })
  
  # Display data previews
  output$respPreview <- renderDT({
    req(values$respData)
    datatable(values$respData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$qMatrixPreview <- renderDT({
    req(values$qMatrix)
    if (!is.null(values$attributeNames)) {
      colnames(values$qMatrix) <- values$attributeNames
    }
    datatable(values$qMatrix, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Run GDINA model
  observeEvent(input$runModel, {
    req(values$respData, values$qMatrix)
    
    withProgress(message = 'Running model...', {
      tryCatch({
        # Convert data to needed format
        resp_matrix <- as.matrix(values$respData)
        q_matrix <- as.matrix(values$qMatrix)
        
        # Set column names for Q-matrix if they don't exist
        if (is.null(colnames(q_matrix))) {
          colnames(q_matrix) <- values$attributeNames
        }
        
        # Run the model
        values$modelResult <- GDINA::GDINA(
          dat = resp_matrix, 
          Q = q_matrix, 
          model = input$modelType,
          method = "MMLE",  # Maximum Marginal Likelihood Estimation
          control = list(
            maxitr = input$maxitr,
            conv.crit = input$convMethod
          )
        )
        
        values$modelReady <- TRUE
        
        # Update student selection dropdown
        updateSelectInput(session, "studentSelect", 
                          choices = paste("Individual", 1:nrow(resp_matrix)))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE
        ))
        values$modelReady <- FALSE
      })
    })
  })
  
  # Update model ready status
  output$modelReady <- reactive({
    return(values$modelReady)
  })
  
  outputOptions(output, "modelReady", suspendWhenHidden = FALSE)
  
  # Model summary output
  output$modelSummary <- renderPrint({
    req(values$modelResult)
    summary(values$modelResult)
  })
  
  # Item parameters
  output$itemParam <- renderDT({
    req(values$modelResult)
    item_param <- extract.itemparm(values$modelResult)
    datatable(item_param, options = list(scrollX = TRUE))
  })
  
  # Attribute probability plot
  output$attrProbPlot <- renderPlot({
    req(values$modelResult)
    attr_prob <- personparm(values$modelResult)
    attr_mean <- colMeans(attr_prob)
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == length(attr_mean)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:length(attr_mean))
    }
    
    df <- data.frame(Attribute = attr_names, Probability = attr_mean)
    
    ggplot(df, aes(x = Attribute, y = Probability, fill = Attribute)) +
      geom_bar(stat = "identity") +
      ylim(0, 1) +
      theme_minimal() +
      labs(title = "Average Attribute Mastery Probability",
           y = "Probability", x = "Attribute") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Radar chart for attribute probabilities
  output$radarChart <- renderPlot({
    req(values$modelResult)
    attr_prob <- personparm(values$modelResult)
    attr_mean <- colMeans(attr_prob)
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == length(attr_mean)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:length(attr_mean))
    }
    
    # Create data frame for radar chart
    radar_data <- data.frame(t(attr_mean))
    colnames(radar_data) <- attr_names
    
    # Add max and min rows required by fmsb package
    radar_data <- rbind(rep(1,length(attr_mean)), rep(0,length(attr_mean)), radar_data)
    
    # Create radar chart
    par(mar = c(1, 1, 2, 1))
    radarchart(
      radar_data, 
      axistype = 1,
      pcol = rgb(0.2, 0.5, 0.8, 0.9),
      pfcol = rgb(0.2, 0.5, 0.8, 0.5),
      plwd = 2,
      cglcol = "gray",
      cglty = 1,
      axislabcol = "gray30",
      caxislabels = seq(0, 1, 0.25),
      title = "Attribute Mastery Radar Chart"
    )
    
    # Add text labels for the probabilities
    for(i in 1:length(attr_mean)) {
      value_text <- sprintf("%.2f", attr_mean[i])
      pos_angle <- (2*pi/length(attr_mean))*(i-1) 
      x_pos <- 0.8 * attr_mean[i] * sin(pos_angle)
      y_pos <- 0.8 * attr_mean[i] * cos(pos_angle)
      text(x_pos, y_pos, value_text, cex = 0.8, col = "navy")
    }
  })
  
  # Individual radar chart
  output$individualRadarChart <- renderPlot({
    req(values$modelResult, input$studentSelect)
    
    # Get individual probabilities
    attr_prob <- personparm(values$modelResult)
    student_idx <- as.numeric(sub("Individual ", "", input$studentSelect))
    
    if (student_idx > nrow(attr_prob) || student_idx < 1) {
      return(NULL)
    }
    
    indiv_prob <- attr_prob[student_idx, ]
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == length(indiv_prob)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:length(indiv_prob))
    }
    
    # Create data frame for radar chart
    radar_data <- data.frame(t(indiv_prob))
    colnames(radar_data) <- attr_names
    
    # Add max and min rows required by fmsb package
    radar_data <- rbind(rep(1,length(indiv_prob)), rep(0,length(indiv_prob)), radar_data)
    
    # Create radar chart
    par(mar = c(1, 1, 2, 1))
    radarchart(
      radar_data, 
      axistype = 1,
      pcol = rgb(0.8, 0.2, 0.5, 0.9),
      pfcol = rgb(0.8, 0.2, 0.5, 0.5),
      plwd = 2,
      cglcol = "gray",
      cglty = 1,
      axislabcol = "gray30",
      caxislabels = seq(0, 1, 0.25),
      title = paste("Individual Profile:", input$studentSelect)
    )
    
    # Add text labels for the probabilities
    for(i in 1:length(indiv_prob)) {
      value_text <- sprintf("%.2f", indiv_prob[i])
      pos_angle <- (2*pi/length(indiv_prob))*(i-1) 
      x_pos <- 0.8 * indiv_prob[i] * sin(pos_angle)
      y_pos <- 0.8 * indiv_prob[i] * cos(pos_angle)
      text(x_pos, y_pos, value_text, cex = 0.8, col = "darkred")
    }
  })
  
  # Individual attribute mastery
  output$individualMastery <- renderPrint({
    req(values$modelResult, input$studentSelect)
    
    # Get individual mastery patterns (MAP estimates)
    attr_pattern <- personparm(values$modelResult, what = "MAP")
    student_idx <- as.numeric(sub("Individual ", "", input$studentSelect))
    
    if (student_idx > nrow(attr_pattern) || student_idx < 1) {
      return("Individual data not available")
    }
    
    indiv_pattern <- attr_pattern[student_idx, ]
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == length(indiv_pattern)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:length(indiv_pattern))
    }
    
    # Create summary
    summary_text <- "Attribute Mastery Status:\n\n"
    for (i in 1:length(indiv_pattern)) {
      status <- ifelse(indiv_pattern[i] == 1, "Mastered", "Not Mastered")
      summary_text <- paste0(summary_text, attr_names[i], ": ", status, "\n")
    }
    
    summary_text
  })
  
  # Profile summary for individual
  output$profileSummary <- renderPrint({
    req(values$modelResult, input$studentSelect)
    
    # Get individual probabilities
    attr_prob <- personparm(values$modelResult)
    student_idx <- as.numeric(sub("Individual ", "", input$studentSelect))
    
    if (student_idx > nrow(attr_prob) || student_idx < 1) {
      return("Profile data not available")
    }
    
    indiv_prob <- attr_prob[student_idx, ]
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == length(indiv_prob)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:length(indiv_prob))
    }
    
    # Get MAP pattern
    attr_pattern <- personparm(values$modelResult, what = "MAP")
    indiv_pattern <- attr_pattern[student_idx, ]
    
    # Create general cognitive profile summary
    avg_prob <- mean(indiv_prob)
    strongest_attr <- attr_names[which.max(indiv_prob)]
    weakest_attr <- attr_names[which.min(indiv_prob)]
    mastery_count <- sum(indiv_pattern)
    total_attrs <- length(indiv_pattern)
    
    summary_text <- paste0(
      "Cognitive Profile Summary for ", input$studentSelect, ":\n\n",
      "Overall Mastery Probability: ", round(avg_prob * 100, 1), "%\n",
      "Attributes Mastered: ", mastery_count, " of ", total_attrs, " (", 
      round(mastery_count/total_attrs * 100, 1), "%)\n\n",
      "Strongest Attribute: ", strongest_attr, " (", round(max(indiv_prob) * 100, 1), "%)\n",
      "Weakest Attribute: ", weakest_attr, " (", round(min(indiv_prob) * 100, 1), "%)\n\n"
    )
    
    # Add specific feedback based on computational thinking context if applicable
    if (!is.null(values$attributeNames) && 
        all(values$attributeNames %in% c("Decomposition", "Abstraction", "Algorithmic", "Pattern", "Automation"))) {
      
      summary_text <- paste0(summary_text, "Computational Thinking Profile:\n\n")
      
      if (indiv_pattern["Decomposition"] == 1) {
        summary_text <- paste0(summary_text, "✓ Can break down complex problems into manageable parts\n")
      } else {
        summary_text <- paste0(summary_text, "✗ Needs improvement in breaking down complex problems\n")
      }
      
      if (indiv_pattern["Abstraction"] == 1) {
        summary_text <- paste0(summary_text, "✓ Can identify patterns and generalizations\n")
      } else {
        summary_text <- paste0(summary_text, "✗ Needs improvement in identifying patterns and generalizations\n")
      }
      
      if (indiv_pattern["Algorithmic"] == 1) {
        summary_text <- paste0(summary_text, "✓ Can create step-by-step solutions\n")
      } else {
        summary_text <- paste0(summary_text, "✗ Needs improvement in creating step-by-step solutions\n")
      }
      
      if (indiv_pattern["Pattern"] == 1) {
        summary_text <- paste0(summary_text, "✓ Can recognize patterns in data\n")
      } else {
        summary_text <- paste0(summary_text, "✗ Needs improvement in recognizing patterns in data\n")
      }
      
      if (indiv_pattern["Automation"] == 1) {
        summary_text <- paste0(summary_text, "✓ Can use computers to automate tasks\n")
      } else {
        summary_text <- paste0(summary_text, "✗ Needs improvement in using computers to automate tasks\n")
      }
    }
    
    summary_text
  })
  
  # Individual pattern table
  output$individualPattern <- renderDT({
    req(values$modelResult, input$studentSelect)
    
    # Get individual mastery patterns and probabilities
    attr_pattern <- personparm(values$modelResult, what = "MAP")
    attr_prob <- personparm(values$modelResult)
    student_idx <- as.numeric(sub("Individual ", "", input$studentSelect))
    
    if (student_idx > nrow(attr_pattern) || student_idx < 1) {
      return(NULL)
    }
    
    indiv_pattern <- attr_pattern[student_idx, , drop = FALSE]
    indiv_prob <- attr_prob[student_idx, , drop = FALSE]
    
    if (!is.null(values$attributeNames) && length(values$attributeNames) == ncol(indiv_pattern)) {
      attr_names <- values$attributeNames
    } else {
      attr_names <- paste("Attribute", 1:ncol(indiv_pattern))
    }
    
    colnames(indiv_pattern) <- attr_names
    colnames(indiv_prob) <- attr_names
    
    # Create combined data
    result_df <- data.frame(
      Attribute = attr_names,
      Status = ifelse(indiv_pattern[1,] == 1, "Mastered", "Not Mastered"),
      Probability = round(indiv_prob[1,] * 100, 1)
    )
    
    datatable(result_df, options = list(dom = 't')) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c("Mastered", "Not Mastered"), 
          c("#d4edda", "#f8d7da")
        )
      ) %>%
      formatStyle(
        'Probability',
        background = styleColorBar(c(0, 100), '#5bc0de'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Item fit
  output$itemFit <- renderDT({
    req(values$modelResult)
    item_fit <- itemfit(values$modelResult)
    datatable(item_fit, options = list(scrollX = TRUE))
  })
  
  # Attribute pattern
  output$attrPattern <- renderDT({
    req(values$modelResult)
    attr_pattern <- personparm(values$modelResult, what = "MAP")
    
    if (!is.null(values$attributeNames) && ncol(attr_pattern) == length(values$attributeNames)) {
      colnames(attr_pattern) <- values$attributeNames
    }
    
    datatable(attr_pattern, options = list(scrollX = TRUE, 
                                           columnDefs = list(list(
                                             targets = 0:(ncol(attr_pattern)-1),
                                             render = JS("function(data, type, row, meta) {
                                                        return data == 1 ? 'Mastered' : 'Not Mastered';
                                                        }")
                                           ))))
  })
  
  # Attribute pattern plot
  output$attrPatternPlot <- renderPlot({
    req(values$modelResult)
    attr_pattern <- personparm(values$modelResult, what = "MAP")
    
    # Convert to string representation for plotting frequency
    pattern_strings <- apply(attr_pattern, 1, function(row) paste(row, collapse = ""))
    pattern_counts <- as.data.frame(table(pattern_strings))
    
    # Sort by frequency for better visualization
    pattern_counts <- pattern_counts[order(pattern_counts$Freq, decreasing = TRUE), ]
    
    # Limit to top 20 patterns if too many
    if(nrow(pattern_counts) > 20) {
      pattern_counts <- pattern_counts[1:20, ]
    }
    
    ggplot(pattern_counts, aes(x = reorder(pattern_strings, -Freq), y = Freq, fill = pattern_strings)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Attribute Pattern Distribution",
           y = "Frequency", x = "Attribute Pattern") +
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Download results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("GDINA-results-", Sys.Date(), ".RData", sep = "")
    },
    content = function(file) {
      model_results <- values$modelResult
      save(model_results, file = file)
    }
  )
  
  # Download radar chart
  output$downloadRadarChart <- downloadHandler(
    filename = function() {
      paste("radar-chart-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600, res = 100)
      
      # Get attribute probabilities
      attr_prob <- personparm(values$modelResult)
      attr_mean <- colMeans(attr_prob)
      
      if (!is.null(values$attributeNames) && length(values$attributeNames) == length(attr_mean)) {
        attr_names <- values$attributeNames
      } else {
        attr_names <- paste("Attribute", 1:length(attr_mean))
      }
      
      # Create data frame for radar chart
      radar_data <- data.frame(t(attr_mean))
      colnames(radar_data) <- attr_names
      
      # Add max and min rows required by fmsb package
      radar_data <- rbind(rep(1, length(attr_mean)), rep(0, length(attr_mean)), radar_data)
      
      # Create radar chart
      par(mar = c(1, 1, 2, 1))
      radarchart(
        radar_data, 
        axistype = 1,
        pcol = rgb(0.2, 0.5, 0.8, 0.9),
        pfcol = rgb(0.2, 0.5, 0.8, 0.5),
        plwd = 2,
        cglcol = "gray",
        cglty = 1,
        axislabcol = "gray30",
        caxislabels = seq(0, 1, 0.25),
        title = "Attribute Mastery Radar Chart"
      )
      
      # Add text labels for the probabilities
      for(i in 1:length(attr_mean)) {
        value_text <- sprintf("%.2f", attr_mean[i])
        pos_angle <- (2*pi/length(attr_mean))*(i-1) 
        x_pos <- 0.8 * attr_mean[i] * sin(pos_angle)
        y_pos <- 0.8 * attr_mean[i] * cos(pos_angle)
        text(x_pos, y_pos, value_text, cex = 0.8, col = "navy")
      }
      
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
