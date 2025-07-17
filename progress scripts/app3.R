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
      helpText("Upload a CSV with columns: Activity, Start, End, and optional Group"),
      fileInput("file", "Choose CSV File", accept = ".csv"),
      
      numericInput("binMonths", "Tick Interval (months)", value = 3, min = 1, max = 12),
      
      selectInput(
        "startMonth", "Start date bin at:", 
        choices = setNames(1:12, month.name),
        selected = 1
      ),
      
      textInput("customTitle", "Chart Title", "Gantt Chart"),
      
      selectInput("colorBy", "Color bars by:", choices = c("Group", "Year")),
      
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
    
    num_activities <- nrow(df)
    base_text_size <- 13
    # Gradual scale down but minimum 8 pt font size
    text_size <- max(8, base_text_size - 0.3 * (num_activities - 10))
    
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
        plot.title = element_text(hjust = 0.5, size = text_size + 2)
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
      
      num_activities <- nrow(df)
      base_text_size <- 13
      text_size <- max(8, base_text_size - 0.3 * (num_activities - 10))
      
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
          plot.title = element_text(hjust = 0.5, size = text_size + 2)
        )
      
      ggsave(file, plot = g, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)
