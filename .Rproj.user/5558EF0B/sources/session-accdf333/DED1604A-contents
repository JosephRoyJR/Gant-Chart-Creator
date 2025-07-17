library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(RColorBrewer)

ui <- fluidPage(
  titlePanel("Interactive Gantt Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(HTML("<b style='font-size: 16px;'>Upload a CSV with columns: 'Activity', 'Start', 'End', and optional 'Group' (for custom colours)</b>")),
      
      # Sample CSV section
      tags$details(
        tags$summary("Show sample CSV format", style = "cursor: pointer; color: #0066cc; font-weight: bold;"),
        tags$div(
          style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;",
          tags$table(
            style = "width: 100%; border-collapse: collapse; font-family: monospace; font-size: 12px;",
            tags$tr(
              style = "background-color: #e9ecef; font-weight: bold;",
              tags$td("Activity", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Start", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("End", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Group", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              tags$td("Project A Planning", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("01/01/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("31/01/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Research", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              style = "background-color: #f8f9fa;",
              tags$td("Project A Data Collection", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("01/02/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("30/04/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Research", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              tags$td("Project B Planning", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("15/01/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("15/02/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Research", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              style = "background-color: #f8f9fa;",
              tags$td("Project A Analysis", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("01/04/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("31/05/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Analysis", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              tags$td("Project B Data Collection", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("16/02/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("15/05/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Research", style = "padding: 8px; border: 1px solid #dee2e6;")
            ),
            tags$tr(
              style = "background-color: #f8f9fa;",
              tags$td("Project B Analysis", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("01/05/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("30/06/2025", style = "padding: 8px; border: 1px solid #dee2e6;"),
              tags$td("Analysis", style = "padding: 8px; border: 1px solid #dee2e6;")
            )
          ),
          tags$p(
            style = "margin-top: 10px; font-size: 12px; color: #666;",
            "Note: Use dd/mm/yyyy date format. Group column is optional."
          )
        )
      ),
      
      fileInput("file", "Choose CSV File", accept = ".csv"),
      
      numericInput("binMonths", "Tick Interval (months)", value = 3, min = 1, max = 12),
      
      selectInput(
        "startMonth", "Start date bin at:", 
        choices = setNames(1:12, month.name),
        selected = 1
      ),
      
      textInput("customTitle", "Chart Title", "Gantt Chart"),
      
      selectInput("colorBy", "Color bars by:", choices = c("Group", "Year")),
      
      # New font size controls
      HTML("<h5><b>Font Size Settings</b></h5>"),
      checkboxInput("autoFontSize", "Auto-scale font size", value = TRUE),
      conditionalPanel(
        condition = "!input.autoFontSize",
        numericInput("manualFontSize", "Manual font size", value = 12, min = 6, max = 24)
      ),
      
      # Grid line controls
      HTML("<h5><b>Grid Line Settings</b></h5>"),
      checkboxInput("showMajorGrid", "Show major grid lines", value = TRUE),
      checkboxInput("showMinorGrid", "Show minor grid lines", value = FALSE),
      
      actionButton("generate", "Generate Gantt Chart", class = "btn-primary"),
      
      br(), br(),
      
      downloadButton("downloadPlot", "Download Gantt Chart (PNG)"),
      
      br(), br(),
      
      helpText("Author: Joseph Roy (2025)")
    ),
    
    mainPanel(
      plotOutput("ganttPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  gantt_data <- reactiveVal()
  
  observeEvent(input$generate, {
    req(input$file)
    
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    # Validate required columns
    if (!all(c("Activity", "Start", "End") %in% names(df))) {
      showNotification("CSV must include 'Activity', 'Start', and 'End' columns.", type = "error")
      return()
    }
    
    # Parse dates (assuming dd/mm/yyyy format)
    df <- df %>%
      mutate(
        Start = dmy(Start),
        End = dmy(End),
        Group = if ("Group" %in% names(df)) as.character(Group) else "Other",
        Year = year(Start),
        Activity = factor(Activity, levels = rev(unique(Activity)))
      )
    
    if (any(is.na(df$Start)) || any(is.na(df$End))) {
      showNotification("Date parsing failed. Please use format dd/mm/yyyy.", type = "error")
      return()
    }
    
    gantt_data(df)
  })
  
  # Function to calculate font size
  calculate_font_size <- function(df) {
    if (input$autoFontSize) {
      num_activities <- nrow(df)
      base_text_size <- 13
      # Gradual scale down but minimum 8 pt font size
      max(8, base_text_size - 0.3 * (num_activities - 10))
    } else {
      input$manualFontSize
    }
  }
  
  output$ganttPlot <- renderPlot({
    req(gantt_data())
    df <- gantt_data()
    
    start_year <- year(min(df$Start))
    anchor_month <- as.numeric(input$startMonth)
    min_date <- as.Date(sprintf("%d-%02d-01", start_year, anchor_month))
    if (min_date > min(df$Start)) {
      min_date <- min_date %m-% months(input$binMonths)
    }
    
    max_date <- ceiling_date(max(df$End), unit = "month")
    x_breaks <- seq(min_date, max_date, by = paste(input$binMonths, "months"))
    
    text_size <- calculate_font_size(df)
    
    color_var <- if (input$colorBy == "Group") df$Group else as.factor(df$Year)
    
    color_levels <- unique(color_var)
    palette <- brewer.pal(min(length(color_levels), 8), "Set2")
    color_map <- setNames(palette[1:length(color_levels)], color_levels)
    
    ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = color_var)) +
      geom_segment(size = 6) +
      geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%b\n%Y",
        expand = expansion(mult = c(0.01, 0.05))
      ) +
      scale_color_manual(
        values = color_map,
        name = input$colorBy
      ) +
      labs(
        title = input$customTitle,
        x = "Date",
        y = "Activity"
      ) +
      theme_minimal(base_size = text_size) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = text_size),
        plot.title = element_text(hjust = 0.5, size = text_size + 2),
        panel.grid.major = if (input$showMajorGrid) element_line() else element_blank(),
        panel.grid.minor = if (input$showMinorGrid) element_line() else element_blank()
      )
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("gantt_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(gantt_data())
      df <- gantt_data()
      
      start_year <- year(min(df$Start))
      anchor_month <- as.numeric(input$startMonth)
      min_date <- as.Date(sprintf("%d-%02d-01", start_year, anchor_month))
      if (min_date > min(df$Start)) {
        min_date <- min_date %m-% months(input$binMonths)
      }
      
      max_date <- ceiling_date(max(df$End), unit = "month")
      x_breaks <- seq(min_date, max_date, by = paste(input$binMonths, "months"))
      
      text_size <- calculate_font_size(df)
      
      color_var <- if (input$colorBy == "Group") df$Group else as.factor(df$Year)
      
      color_levels <- unique(color_var)
      palette <- brewer.pal(min(length(color_levels), 8), "Set2")
      color_map <- setNames(palette[1:length(color_levels)], color_levels)
      
      g <- ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = color_var)) +
        geom_segment(size = 6) +
        geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
        scale_x_date(
          breaks = x_breaks,
          date_labels = "%b\n%Y",
          expand = expansion(mult = c(0.01, 0.05))
        ) +
        scale_color_manual(
          values = color_map,
          name = input$colorBy
        ) +
        labs(
          title = input$customTitle,
          x = "Date",
          y = "Activity"
        ) +
        theme_minimal(base_size = text_size) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = text_size),
          plot.title = element_text(hjust = 0.5, size = text_size + 2),
          panel.grid.major = if (input$showMajorGrid) element_line() else element_blank(),
          panel.grid.minor = if (input$showMinorGrid) element_line() else element_blank()
        )
      
      ggsave(file, plot = g, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)