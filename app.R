# -------------------------
# Load libraries
# -------------------------
library(shiny)
library(ggplot2)
library(GGally)
library(reshape2)

# Load your data
df_modelling <- read.csv("df_modelling.csv")
df_modelling$target <- as.factor(df_modelling$target)
load("cor_matrix.RData")
load("model_metrics.RData")

# Load Feature Importances CSV
feature_importance_rf <- read.csv("feature_importances_rf.csv", row.names = 1)

# -------------------------
# Define UI
# -------------------------
ui <- navbarPage(
  "Lending Club Analysis - Credit Crunchers",
  
  
  
  tabPanel(
    "Exploratory Data Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "eda_select",
          label = "Select Visualization:",
          choices = c("Correlation Heatmap", "Boxplot", "Histogram & ScatterPlot"),
          selected = "Correlation Heatmap"
        ),
        uiOutput("feature_selector")
      ),
      mainPanel(
        plotOutput("eda_plot", height = "800px")
      )
    )
  ),
  
  tabPanel(
    "SMOTE & Feature Importance",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "smote_or_importance",
          label = "Select View:",
          choices = c("SMOTE", "Feature Importance"),
          selected = "SMOTE"
        ),
        uiOutput("smote_buttons")
      ),
      mainPanel(
        plotOutput("smote_importance_plot", height = "800px")
      )
    )
  ), 
  tabPanel(
    "Model Evaluation",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "model_select",
          label = "Select Model:",
          choices = names(model_metrics),
          selected = "XGBoost"
        )
      ),
      mainPanel(
        h3(textOutput("model_title")),
        tableOutput("metric_table")
      )
    )
  )
)

# -------------------------
# Define Server
# -------------------------
server <- function(input, output, session) {
  
  values <- reactiveValues(
    show_scaled = FALSE,
    smote_applied = FALSE
  )
  
  # Dynamic UI for EDA feature selection
  output$feature_selector <- renderUI({
    if (input$eda_select == "Histogram & ScatterPlot") {
      tagList(
        selectInput(
          inputId = "selected_features",
          label = "Select 2 Features:",
          choices = sort(setdiff(names(df_modelling), "target")),
          selected = c("loan_amnt", "int_rate"),
          multiple = TRUE
        )
      )
    } else if (input$eda_select == "Boxplot") {
      tagList(
        actionButton("scale_data", label = ifelse(values$show_scaled, "Hide Scaled Boxplot", "Show After Scaling Boxplot"))
      )
    }
  })
  
  # Dynamic SMOTE Buttons
  output$smote_buttons <- renderUI({
    if (input$smote_or_importance == "SMOTE") {
      tagList(
        actionButton(
          inputId = "apply_smote",
          label = ifelse(values$smote_applied, "SMOTE Applied ✅", "Apply SMOTE"),
          disabled = ifelse(values$smote_applied, TRUE, FALSE)
        ),
        br(), br(),
        actionButton("reset_smote", "Reset")
      )
    }
  })
  
  # Toggle for Boxplot scaling
  observeEvent(input$scale_data, {
    values$show_scaled <- !values$show_scaled
  })
  
  # Toggle for SMOTE
  observeEvent(input$apply_smote, {
    values$smote_applied <- TRUE
  })
  
  observeEvent(input$reset_smote, {
    values$smote_applied <- FALSE
  })
  
  # Model Evaluation Outputs
  selected_model <- reactive({
    model_metrics[[input$model_select]]
  })
  
  output$model_title <- renderText({
    model_name <- input$model_select
    accuracy <- selected_model()$accuracy
    paste0(model_name, " Accuracy: ", round(accuracy, 4))
  })
  
  output$model_table <- renderTable({
    selected_model()$table
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$metric_table <- renderTable({
    selected_model()$table
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # EDA Tab Outputs
  output$eda_plot <- renderPlot({
    if (input$eda_select == "Correlation Heatmap") {
      
      cor_melt <- melt(cor_matrix)
      ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0, limit = c(-1,1), space = "Lab",
                             name = "Correlation") +
        theme_minimal(base_size = 18) +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        coord_fixed()
      
    } else if (input$eda_select == "Histogram & ScatterPlot") {
      
      req(input$selected_features)
      
      if (length(input$selected_features) == 2) {
        
        ggplot(df_modelling, aes_string(x = input$selected_features[1], 
                                        y = input$selected_features[2], 
                                        color = "target")) +
          geom_point(alpha = 0.6, size = 2) +
          theme_minimal(base_size = 16) +
          labs(
            x = input$selected_features[1],
            y = input$selected_features[2],
            title = paste(input$selected_features[1], "vs", input$selected_features[2])
          ) +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank()
          )
        
      } else if (length(input$selected_features) == 1) {
        
        ggplot(df_modelling, aes_string(x = input$selected_features[1], fill = "target")) +
          geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
          theme_minimal(base_size = 16) +
          labs(
            x = input$selected_features[1],
            y = "Count",
            title = paste("Distribution of", input$selected_features[1])
          ) +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank()
          )
        
      } else {
        
        plot.new()
        text(0.5, 0.5, "Please select 1 or 2 features.", col = "red", cex = 2, font = 2)
      }
    }
    else if (input$eda_select == "Boxplot") {
      
      par(mfrow = c(ifelse(values$show_scaled, 2, 1), 1))
      
      ## Before Scaling
      boxplot(df_modelling[, sapply(df_modelling, is.numeric)], 
              las = 2, 
              main = "Boxplot Before Scaling",
              cex.axis = 0.8,
              col = "skyblue")
      
      ## After Scaling
      if (values$show_scaled) {
        numeric_cols <- names(df_modelling)[sapply(df_modelling, is.numeric)]
        binary_cols <- sapply(df_modelling[, numeric_cols], function(x) all(x %in% c(0,1)))
        continuous_cols <- numeric_cols[!binary_cols]
        
        scaler <- scale(df_modelling[, continuous_cols])
        df_modelling_scaled <- df_modelling
        df_modelling_scaled[, continuous_cols] <- scaler
        
        boxplot(df_modelling_scaled[, continuous_cols], 
                las = 2, 
                main = "Boxplot After Scaling",
                cex.axis = 0.8,
                col = "lightgreen")
      }
      
      par(mfrow = c(1,1))
    }
  })
  
  # SMOTE & Feature Importance Outputs
  output$smote_importance_plot <- renderPlot({
    if (input$smote_or_importance == "SMOTE") {
      if (!values$smote_applied) {
        ## Before SMOTE - Original
        target_counts <- table(df_modelling$target)
        
        pie(
          target_counts,
          labels = paste0(names(target_counts), " (", round(100 * target_counts / sum(target_counts), 1), "%)"),
          col = c("green", "red"),
          main = "Original Target Class Distribution",
          radius = 1
        )
        
      } else {
        ## After SMOTE
        par(mfrow = c(2,1))
        
        ## Before SMOTE
        target_counts <- table(df_modelling$target)
        pie(
          target_counts,
          labels = paste0(names(target_counts), " (", round(100 * target_counts / sum(target_counts), 1), "%)"),
          col = c("green", "red"),
          main = "Original Target Class Distribution",
          radius = 1
        )
        
        ## After SMOTE
        after_counts <- c(500, 500)
        names(after_counts) <- c("Class 0", "Class 1")
        
        pie(
          after_counts,
          labels = paste0(names(after_counts), " (50.0%)"),
          col = c("green", "red"),
          main = "After SMOTE Target Class Distribution",
          radius = 1
        )
        
        par(mfrow = c(1,1))
      }
      
    } else if (input$smote_or_importance == "Feature Importance") {
      
      # Sort feature importances descending
      sorted_feature_importance <- feature_importance_rf[order(feature_importance_rf[,1]), , drop=FALSE]
      
      # Set larger left margin to display full feature names
      par(mar = c(5, 10, 4, 2))
      
      barplot(
        sorted_feature_importance[, 1],
        names.arg = rownames(sorted_feature_importance),
        horiz = TRUE,
        las = 1,
        cex.names = 0.8,
        col = "steelblue",
        xlab = "Importance",
        main = "Random Forest Feature Importance"
      )
    }
  })
}

# -------------------------
# Run the App
# -------------------------
shinyApp(ui = ui, server = server)
