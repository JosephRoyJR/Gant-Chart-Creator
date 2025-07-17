
############## Colour based on date 

library(ggplot2)
library(dplyr)
library(lubridate)

# Read the CSV file
gantt_data <- read.csv("gantt_phd.csv", stringsAsFactors = FALSE)

# Prepare and label data
gantt_data <- gantt_data %>%
  mutate(
    Start = ymd(Start),
    End = ymd(End),
    Start = if_else(Start > End, End, Start),
    End = if_else(Start > End, Start, End),
    
    # Chapter labels based on Activity
    Chapter = case_when(
      grepl("Chapter 1", Activity) ~ "Chapter 1",
      grepl("Chapter 2", Activity) ~ "Chapter 2",
      grepl("Chapter 3", Activity) ~ "Chapter 3",
      grepl("Chapter 4", Activity) ~ "Chapter 4",
      grepl("Chapter 5", Activity) ~ "Chapter 5",
      grepl("thesis", Activity, ignore.case = TRUE) ~ "Final Thesis",
      TRUE ~ "Other"
    ),
    
    Activity = factor(Activity, levels = rev(Activity))  # preserve CSV order
  )

# Set x-axis range
min_date <- floor_date(min(gantt_data$Start), unit = "year")
max_date <- ceiling_date(max(gantt_data$End), unit = "year")

# Breaks every 6 months: June and December
x_breaks <- seq.Date(from = as.Date(paste0(year(min_date), "-06-01")),
                     to = as.Date(paste0(year(max_date), "-12-01")),
                     by = "6 months")

# Plot Gantt chart
ggplot(gantt_data, aes(x = Start, xend = End, y = Activity, yend = Activity, color = Chapter)) +
  geom_segment(size = 6) +
  geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    x = "Date",
    y = "Activity",
    color = "Chapter"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )


############# Colour based on chapter 

library(ggplot2)
library(dplyr)
library(lubridate)

# Read the CSV file
gantt_data <- read.csv("gantt_phd.csv", stringsAsFactors = FALSE)

# Prepare and label data
gantt_data <- gantt_data %>%
  mutate(
    Start = ymd(Start),
    End = ymd(End),
    Start = if_else(Start > End, End, Start),
    End = if_else(Start > End, Start, End),
    
    # Chapter labels based on Activity
    Chapter = case_when(
      grepl("Chapter 1", Activity) ~ "Chapter 1",
      grepl("Chapter 2", Activity) ~ "Chapter 2",
      grepl("Chapter 3", Activity) ~ "Chapter 3",
      grepl("Chapter 4", Activity) ~ "Chapter 4",
      grepl("Chapter 5", Activity) ~ "Chapter 5",
      grepl("thesis", Activity, ignore.case = TRUE) ~ "Final Thesis",
      TRUE ~ "Other"
    ),
    
    Activity = factor(Activity, levels = rev(Activity))  # preserve CSV row order
  )

# Set x-axis range
min_date <- floor_date(min(gantt_data$Start), unit = "year")
max_date <- ceiling_date(max(gantt_data$End), unit = "year")

# Breaks every 6 months: June and December
x_breaks <- seq.Date(from = as.Date(paste0(year(min_date), "-06-01")),
                     to = as.Date(paste0(year(max_date), "-12-01")),
                     by = "6 months")

# Plot Gantt chart
ggplot(gantt_data, aes(x = Start, xend = End, y = Activity, yend = Activity, color = Chapter)) +
  geom_segment(size = 6) +
  geom_vline(xintercept = Sys.Date(), linetype = "dashed", color = "red") +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_color_manual(values = c(
    "Chapter 1" = "#D55E00",
    "Chapter 2" = "#0072B2",
    "Chapter 3" = "#009E73",
    "Chapter 4" = "#CC79A7",
    "Chapter 5" = "brown",
    "Final Thesis" = "#999999",
    "Other" = "#56B4E9"
  )) +
  labs(
    x = "Date",
    y = "Activity",
    color = "Chapter"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

