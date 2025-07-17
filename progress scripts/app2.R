library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

ui <- fluidPage(
  titlePanel("Interactive Gantt Chart Generator"),
  
  tags$div(
    style = "margin-bottom: 20px;",
    p("🔧 To use this Gantt chart tool:"),
    tags$ol(
      tags$li("Either type in your activities and dates OR upload a CSV file with 'Activity', 'Start', 'End' columns."),
      tags$li("Dates must be in format: dd/mm/yyyy."),
      tags$li("Choose the X-axis tick interval in months."),
      tags$li("Click 'Generate Gantt Chart' to view."),
      tags$li("Click 'Download Gantt Chart' to save as PNG.")
    ),
    p(em("Created by Joseph Roy, 2025"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("plotTitle", "Chart Title", "Custom Gantt Chart"),
      fileInput("fileUpload", "Or Upload CSV (Activity, Start, End)", accept = ".csv"),
      textAreaInput("tasks", "Activities", rows = 5, placeholder = "e.g.\nChapter 1\nChapter 2"),
      textAreaInput("starts", "Start Dates (dd/mm/yyyy)", rows = 5, placeholder = "e.g.\n01/01/2025\n01/02/2025"),
      textAreaInput("ends", "End Dates (dd/mm/yyyy)", rows = 5, placeholder = "e.g.\n01/03/2025\n01/04/2025"),
      checkboxInput("preserve_order", "Preserve row order", value = TRUE),
      selectInput(
        "monthBin",
        "X-axis tick interval (months):",
        choices = c("1 month" = 1, "3 months" = 3, "6 months" = 6, "12 months" = 12),
        selected = 6
      ),
      actionButton("generate", "Generate Gantt Chart", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadPlot", "Download Gantt Chart")
    ),
    
    mainPanel(
      h4("Gantt Chart"),
      plotOutput("ganttPlot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  originalOrder <- reactiveVal(NULL)
  
  parse_dates_safe <- function(x) {
    x <- trimws(x)
    parsed <- suppressWarnings(lubridate::parse_date_time(x, orders = c("dmy", "ymd", "mdy")))
    if (any(is.na(parsed) & x != "")) {
      showNotification("⚠️ Some dates could not be parsed. Please use dd/mm/yyyy format.", type = "error")
    }
    return(parsed)
  }
  
  observeEvent(input$generate, {
    if (!is.null(input$fileUpload)) {
      df <- read_csv(input$fileUpload$datapath, show_col_types = FALSE)
      if (!all(c("Activity", "Start", "End") %in% names(df))) {
        showNotification("CSV must contain columns: Activity, Start, End", type = "error")
        return()
      }
      df <- df %>%
        mutate(
          Start = parse_dates_safe(Start),
          End = parse_dates_safe(End)
        )
    } else {
      tasks <- strsplit(input$tasks, "\n")[[1]]
      starts <- strsplit(input$starts, "\n")[[1]]
      ends <- strsplit(input$ends, "\n")[[1]]
      
      if (length(tasks) != length(starts) || length(tasks) != length(ends)) {
        showNotification("Each activity must have a start and end date.", type = "error")
        return()
      }
      
      df <- data.frame(
        Activity = trimws(tasks),
        Start = parse_dates_safe(starts),
        End = parse_dates_safe(ends),
        stringsAsFactors = FALSE
      )
    }
    
    df <- df %>%
      filter(!is.na(Start) & !is.na(End)) %>%
      mutate(
        Start = if_else(Start > End, End, Start),
        End = if_else(Start > End, Start, End),
        Year = year(Start),
        RowID = row_number()
      )
    
    originalOrder(df$RowID)
    
    if (input$preserve_order) {
      df <- df %>% arrange(RowID)
    } else {
      df <- df %>% arrange(Start)
    }
    
    df$Activity <- factor(df$Activity, levels = rev(df$Activity))
    dataInput(df)
  })
  
  output$ganttPlot <- renderPlot({
    req(dataInput())
    df <- dataInput()
    
    df$Start <- as.Date(df$Start)
    df$End <- as.Date(df$End)
    
    min_date <- floor_date(min(df$Start), unit = "month")
    max_date <- ceiling_date(max(df$End), unit = "month")
    
    # Create breaks sequence using selected month interval
    month_interval <- as.numeric(input$monthBin)
    x_breaks <- seq.Date(from = min_date, to = max_date, by = paste0(month_interval, " months"))
    
    ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = as.factor(Year))) +
      geom_segment(size = 6) +
      geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%b\n%Y",
        expand = expansion(mult = c(0.01, 0.05))
      ) +
      labs(
        title = input$plotTitle,
        x = "Date",
        y = "Activity",
        color = "Start Year"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("gantt_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- dataInput()
      
      df$Start <- as.Date(df$Start)
      df$End <- as.Date(df$End)
      
      min_date <- floor_date(min(df$Start), unit = "month")
      max_date <- ceiling_date(max(df$End), unit = "month")
      
      month_interval <- as.numeric(input$monthBin)
      x_breaks <- seq.Date(from = min_date, to = max_date, by = paste0(month_interval, " months"))
      
      g <- ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = as.factor(Year))) +
        geom_segment(size = 6) +
        geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
        scale_x_date(
          breaks = x_breaks,
          date_labels = "%b\n%Y",
          expand = expansion(mult = c(0.01, 0.05))
        ) +
        labs(
          title = input$plotTitle,
          x = "Date",
          y = "Activity",
          color = "Start Year"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      
      ggsave(file, plot = g, width = 12, height = 8, dpi = 300)
    }
  )
}

shinyApp(ui, server)
