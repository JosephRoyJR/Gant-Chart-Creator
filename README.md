# Gant Chart Creator
A simple gantt chart generator from csv

Access online here- https://kgez64-joseph-roy.shinyapps.io/simple_year_coloured_gantt_chart/


# 📊 Interactive Gantt Chart Generator (Shiny App)

This Shiny app allows users to generate and download clean, informative Gantt charts from a CSV file. The charts are customizable by time bins, color groups, and include visual aids like "today" markers. It is especially useful for visualizing timelines such as thesis planning, project schedules, or event roadmaps.

---

## 🔧 Features

- Upload your own CSV file with activity data
- Automatically parses and formats start and end dates
- Customizable chart title
- Choose how bars are colored: by `Group` or `Year`
- Set the number of months between time ticks
- Choose the start month for binning (e.g., January or June)
- Automatically highlights today’s date with a red dashed line
- Download high-quality PNG of the chart with a white background
- Minimalist and user-friendly interface

---

## 📁 CSV Format

Your input CSV file should contain the following **required columns**:

| Column   | Description                                  | Example              |
|----------|----------------------------------------------|----------------------|
| Activity | Name of the task or phase                    | "Literature Review"  |
| Start    | Start date (in `dd/mm/yyyy` format)          | "01/01/2025"         |
| End      | End date (in `dd/mm/yyyy` format)            | "31/03/2025"         |

**Optional column**:

- `Group`: Used for color grouping (e.g., Chapter name, Workstream, Phase)

You can color tasks by either the values in `Group` or automatically by `Year`.

---

## 🚀 Getting Started

### Prerequisites

Install the required R packages:

install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "readr", "RColorBrewer"))


install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "readr", "RColorBrewer"))

Run the App
Clone the repository and run the app in R: file- app.r

Or, simply open app.R in RStudio and click Run App.

📤 Output
The generated Gantt chart is:

High-resolution (PNG format)

White background for compatibility with reports/presentations

Suitable for both screen and print

✍️ Author
Created by Joseph Roy, 2025
PhD in Eco-psychology | Bangor University

This app was vibe coded