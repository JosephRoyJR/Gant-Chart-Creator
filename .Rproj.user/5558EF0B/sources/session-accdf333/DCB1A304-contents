library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)

ui <- fluidPage(
  titlePanel("Interactive Gantt Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter activities and start/end dates (YYYY-MM-DD format):"),
      textAreaInput("tasks", "Activities", rows = 7, placeholder = "e.g.\nChapter 1\nChapter 2"),
      textAreaInput("starts", "Start Dates", rows = 7, placeholder = "e.g.\n2025-01-01\n2025-02-01"),
      textAreaInput("ends", "End Dates", rows = 7, placeholder = "e.g.\n2025-03-01\n2025-04-01"),
      checkboxInput("preserve_order", "Preserve row order", value = TRUE),
      actionButton("generate", "Generate Gantt Chart", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadPlot", "Download Gantt Chart")
    ),
    
    mainPanel(
      h4("Gantt Chart"),
      plotOutput("ganttPlot", height = "500px"),
      br(),
      h4("Edit Data"),
      DTOutput("editableTable")
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  originalOrder <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    tasks <- strsplit(input$tasks, "\n")[[1]]
    starts <- ymd(strsplit(input$starts, "\n")[[1]])
    ends <- ymd(strsplit(input$ends, "\n")[[1]])
    
    if (length(tasks) != length(starts) || length(tasks) != length(ends)) {
      showNotification("Each activity must have a start and end date.", type = "error")
      return()
    }
    
    df <- data.frame(Activity = tasks, Start = starts, End = ends, stringsAsFactors = FALSE)
    
    df <- df %>%
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
  
  output$editableTable <- renderDT({
    req(dataInput())
    datatable(dataInput()[, c("Activity", "Start", "End", "Year")],
              editable = TRUE,
              rownames = FALSE)
  })
  
  observeEvent(input$editableTable_cell_edit, {
    info <- input$editableTable_cell_edit
    df <- dataInput()
    df[info$row, info$col] <- info$value
    
    df$Start <- ymd(df$Start)
    df$End <- ymd(df$End)
    df$Year <- year(df$Start)
    
    if (input$preserve_order) {
      df <- df %>% arrange(factor(RowID, levels = originalOrder()))
    } else {
      df <- df %>% arrange(Start)
    }
    
    df$Activity <- factor(df$Activity, levels = rev(df$Activity))
    dataInput(df)
  })
  
  output$ganttPlot <- renderPlot({
    req(dataInput())
    df <- dataInput()
    
    min_date <- floor_date(min(df$Start), unit = "year")
    max_date <- ceiling_date(max(df$End), unit = "year")
    
    x_breaks <- seq.Date(
      from = as.Date(paste0(year(min_date), "-06-01")),
      to = as.Date(paste0(year(max_date), "-12-01")),
      by = "6 months"
    )
    
    ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = as.factor(Year))) +
      geom_segment(size = 6) +
      geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%b\n%Y",
        expand = expansion(mult = c(0.01, 0.05))
      ) +
      labs(
        title = "Custom Gantt Chart",
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
      
      min_date <- floor_date(min(df$Start), unit = "year")
      max_date <- ceiling_date(max(df$End), unit = "year")
      
      x_breaks <- seq.Date(
        from = as.Date(paste0(year(min_date), "-06-01")),
        to = as.Date(paste0(year(max_date), "-12-01")),
        by = "6 months"
      )
      
      g <- ggplot(df, aes(x = Start, xend = End, y = Activity, yend = Activity, color = as.factor(Year))) +
        geom_segment(size = 6) +
        geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
        scale_x_date(
          breaks = x_breaks,
          date_labels = "%b\n%Y",
          expand = expansion(mult = c(0.01, 0.05))
        ) +
        labs(
          title = "Custom Gantt Chart",
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
