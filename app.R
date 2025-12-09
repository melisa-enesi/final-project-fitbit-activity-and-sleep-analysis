# ============================================================
# Libraries
# ============================================================
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(shinythemes)

# labels for hours in 12-hour clock format (for x-axis)
hour_labels <- format(strptime(0:23, format = "%H"), "%I:00 %p")

# POSIXct times for the slider (dummy date, just to get time-of-day)
hour_times <- as.POSIXct(sprintf("2000-01-01 %02d:00:00", 0:23), tz = "UTC")

# ============================================================
# Load data  (all from Kaggle Fitbit zip, copied into data/)
# ============================================================
daily        <- read_csv("data/dailyActivity_merged.csv")
sleep        <- read_csv("data/sleepDay_merged.csv")
hourly_steps <- read_csv("data/hourlySteps_merged.csv")
hourly_cal   <- read_csv("data/hourlyCalories_merged.csv")
minute_sleep <- read_csv("data/minuteSleep_merged.csv")
weight       <- read_csv("data/weightLogInfo_merged.csv")

# ============================================================
# Clean + merge data
# ============================================================
daily <- daily %>%
  mutate(ActivityDate = mdy(ActivityDate))

sleep <- sleep %>%
  mutate(SleepDay  = mdy_hms(SleepDay),
         SleepDate = as_date(SleepDay))

daily_sleep <- daily %>%
  inner_join(sleep, by = c("Id" = "Id", "ActivityDate" = "SleepDate"))

hourly_steps <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

hourly_cal <- hourly_cal %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

minute_sleep <- minute_sleep %>%
  mutate(date      = mdy_hms(date),
         SleepDate = as_date(date))

weight <- weight %>%
  mutate(Date = mdy_hms(Date))

# IDs
all_ids   <- sort(unique(daily$Id))
sleep_ids <- sort(unique(sleep$Id))

# ============================================================
# Precomputed datasets for plots
# ============================================================

# Weekday vs Weekend Activity
weekday_hourly_steps <- hourly_steps %>%
  mutate(Date   = as_date(ActivityHour),
         Hour   = hour(ActivityHour),
         DayType = if_else(wday(Date, week_start = 1) %in% c(6, 7),
                           "Weekend", "Weekday")) %>%
  group_by(DayType, Hour) %>%
  summarise(AvgSteps = mean(StepTotal, na.rm = TRUE), .groups = "drop")

# Step Leaderboard
leaderboard_all <- daily %>%
  group_by(Id) %>%
  summarise(
    AvgSteps    = mean(TotalSteps, na.rm = TRUE),
    AvgCalories = mean(Calories,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(AvgSteps))

# User Consistency Score
consistency_all <- daily %>%
  group_by(Id) %>%
  summarise(
    AvgSteps = mean(TotalSteps, na.rm = TRUE),
    SdSteps  = sd(TotalSteps,   na.rm = TRUE),
    CV       = SdSteps / AvgSteps,
    .groups  = "drop"
  ) %>%
  filter(!is.na(CV)) %>%
  arrange(CV)

# Sleep Quality Dashboard
sleep_quality_data <- sleep %>%
  mutate(
    HoursAsleep = TotalMinutesAsleep / 60,
    HoursInBed  = TotalTimeInBed / 60,
    Efficiency  = TotalMinutesAsleep / TotalTimeInBed
  )

# Sleep Consistency Calendar
sleep_calendar_all <- sleep %>%
  mutate(
    HoursAsleep = TotalMinutesAsleep / 60,
    Week        = isoweek(SleepDate),
    Weekday     = wday(SleepDate, label = TRUE, week_start = 1)
  )

# ============================================================
# Simulated map locations + real user metrics
# ============================================================
set.seed(123)

user_locations <- tibble(
  Id  = all_ids,
  lat = runif(length(all_ids), min = 25, max = 49),       # fake latitude
  lng = runif(length(all_ids), min = -124, max = -67)     # fake longitude
)

user_summary <- daily %>%
  group_by(Id) %>%
  summarise(
    mean_steps    = mean(TotalSteps, na.rm = TRUE),
    mean_calories = mean(Calories,   na.rm = TRUE),
    .groups = "drop"
  )

user_map_data <- user_locations %>%
  inner_join(user_summary, by = "Id")

max_mean_steps <- max(user_map_data$mean_steps, na.rm = TRUE)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      h2, h3, h4 { font-weight: 600; }
      .small-caption { color: #666; font-size: 0.9em; }
      .well { background-color: #f8f9fa; }
    "))
  ),
  
  titlePanel("Fitbit Activity & Sleep Dashboard"),
  
  tabsetPanel(
    
    # --------------------------------------------------------
    # Tab 1: Sleep vs Calories
    # --------------------------------------------------------
    tabPanel(
      "Sleep vs Calories",
      sidebarLayout(
        sidebarPanel(
          h4("Filter"),
          selectInput(
            "user_select",
            "Choose User ID:",
            choices  = c("All Users", as.character(sleep_ids)),
            selected = "All Users"
          ),
          helpText("Explore relationship between sleep duration and calories burned.")
        ),
        mainPanel(
          plotOutput("sleep_cal_plot", height = "450px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Tab 2: Daily Activity
    # --------------------------------------------------------
    tabPanel(
      "Daily Activity",
      # first row: activity types only
      fluidRow(
        column(
          12,
          h3("Average Minutes by Activity Type"),
          p(
            class = "small-caption",
            "Summarizes how much time users spend in different activity intensities."
          ),
          plotOutput("activity_plot", height = "300px")
        )
      ),
      hr(),
      # second row: leaderboard + calories vs steps
      fluidRow(
        column(
          6,
          h3("Step Leaderboard"),
          p(
            class = "small-caption",
            "Users ranked by average daily steps."
          ),
          wellPanel(
            h4("Options"),
            numericInput(
              "leaderboard_n",
              "Show top N users (by steps):",
              value = 10, min = 3, max = 50
            ),
            helpText("Daily totals come from dailyActivity_merged.")
          ),
          plotOutput("leaderboard_plot", height = "380px")
        ),
        column(
          6,
          h3("Calories vs Steps Efficiency"),
          p(
            class = "small-caption",
            "Shows how daily step counts relate to calories burned."
          ),
          plotOutput("calories_steps_plot", height = "380px")
        )
      ),
      hr(),
      fluidRow(
        column(
          12,
          h3("User Consistency in Daily Steps"),
          p(
            class = "small-caption",
            "Coefficient of variation (standard deviation / mean). Lower values mean more consistent day-to-day steps."
          ),
          plotOutput("consistency_plot", height = "400px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Tab 3: Hourly Patterns
    # --------------------------------------------------------
    tabPanel(
      "Hourly Patterns",
      sidebarLayout(
        sidebarPanel(
          h4("Time of Day Filter"),
          sliderInput(
            "hour_range",
            "Hour Range:",
            min   = hour_times[1],
            max   = hour_times[24],
            value = c(hour_times[9], hour_times[21]),
            step  = 3600,
            timeFormat = "%I:%M %p"
          ),
          radioButtons(
            "hour_metric",
            "Metric:",
            choices = c("Steps", "Calories"),
            selected = "Steps",
            inline = TRUE
          ),
          helpText("Use the slider to zoom into specific parts of the day.")
        ),
        mainPanel(
          h3("Average Activity by Hour of Day"),
          plotOutput("hourly_plot", height = "320px"),
          hr(),
          h3("Weekday vs Weekend Activity"),
          p(
            class = "small-caption",
            "Average hourly steps on weekdays versus weekends."
          ),
          plotOutput("weekday_plot", height = "320px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Tab 4: Sleep Details
    # --------------------------------------------------------
    tabPanel(
      "Sleep Details",
      sidebarLayout(
        sidebarPanel(
          h4("Sleep Options"),
          selectInput(
            "sleep_user_select",
            "Choose User (for calendar view):",
            choices  = as.character(sleep_ids),
            selected = as.character(sleep_ids[1])
          ),
          helpText("Sleep quality plot uses all users; calendar focuses on one user.")
        ),
        mainPanel(
          h3("Sleep Quality Dashboard"),
          p(
            class = "small-caption",
            "Relationship between time in bed and time asleep; points closer to the diagonal are more efficient sleep."
          ),
          plotOutput("sleep_quality_plot", height = "280px"),
          hr(),
          h3("Sleep Consistency Calendar"),
          p(
            class = "small-caption",
            "Calendar-style heatmap for a single user, using week of year on the x-axis.",
            "The sample Fitbit dataset mostly covers March–May, so only those weeks appear; darker tiles mean more hours asleep."
          ),
          plotOutput("sleep_calendar_plot", height = "260px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Tab 5: User Map
    # --------------------------------------------------------
    tabPanel(
      "User Map",
      sidebarLayout(
        sidebarPanel(
          h4("Map Controls"),
          sliderInput(
            "min_steps",
            "Minimum Avg Daily Steps:",
            min   = 0,
            max   = max_mean_steps,
            value = 0,
            step  = 1000
          ),
          selectInput(
            "map_metric",
            "Color by:",
            choices = c("Average Steps" = "steps",
                        "Average Calories" = "calories"),
            selected = "steps"
          ),
          helpText("Filter to more active users and switch what the colors represent.")
        ),
        mainPanel(
          leafletOutput("user_map", height = "500px"),
          br(),
          p(
            "Note: The original Fitbit dataset does not include GPS coordinates.",
            "User locations on this map are simulated, but circle size and color show",
            "each user's average daily activity based on the real data."
          )
        )
      )
    )
    
  ) # end tabsetPanel
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Sleep vs Calories
  # ----------------------------------------------------------
  output$sleep_cal_plot <- renderPlot({
    
    data <- daily_sleep
    
    if (input$user_select != "All Users") {
      data <- data %>%
        filter(Id == as.numeric(input$user_select))
    }
    
    ggplot(data, aes(x = TotalMinutesAsleep, y = Calories)) +
      geom_point(alpha = 0.6, color = "#2c7fb8") +
      geom_smooth(method = "lm", se = FALSE, color = "#d95f0e", linewidth = 1.1) +
      labs(
        title = "Sleep vs Calories Burned",
        x = "Total Minutes Asleep",
        y = "Calories Burned"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Activity types bar plot
  # ----------------------------------------------------------
  output$activity_plot <- renderPlot({
    
    activity_summary <- daily %>%
      summarise(
        VeryActive    = mean(VeryActiveMinutes,    na.rm = TRUE),
        FairlyActive  = mean(FairlyActiveMinutes,  na.rm = TRUE),
        LightlyActive = mean(LightlyActiveMinutes, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "ActivityType",
        values_to = "Minutes"
      )
    
    ggplot(activity_summary, aes(x = ActivityType, y = Minutes, fill = ActivityType)) +
      geom_col(width = 0.6) +
      labs(
        x = "Activity Type",
        y = "Average Minutes per Day"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  # ----------------------------------------------------------
  # Step Leaderboard
  # ----------------------------------------------------------
  output$leaderboard_plot <- renderPlot({
    
    n <- input$leaderboard_n
    
    data <- leaderboard_all %>%
      slice_head(n = n) %>%
      mutate(Id = as.factor(Id)) %>%
      arrange(AvgSteps) %>%
      mutate(Id = fct_inorder(Id))
    
    ggplot(data, aes(x = Id, y = AvgSteps)) +
      geom_col(fill = "#377eb8") +
      coord_flip() +
      labs(
        x = "User ID",
        y = "Average Daily Steps"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Calories vs Steps Efficiency
  # ----------------------------------------------------------
  output$calories_steps_plot <- renderPlot({
    
    ggplot(daily, aes(x = TotalSteps, y = Calories)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      labs(
        x = "Total Steps per Day",
        y = "Calories per Day"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # User Consistency Score
  # ----------------------------------------------------------
  output$consistency_plot <- renderPlot({
    
    data <- consistency_all %>%
      slice_head(n = 15) %>%      # show most consistent 15
      mutate(Id = as.factor(Id)) %>%
      arrange(CV) %>%
      mutate(Id = fct_inorder(Id))
    
    ggplot(data, aes(x = Id, y = CV)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "User ID",
        y = "Coefficient of Variation in Steps"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Hourly steps / calories line plot
  # ----------------------------------------------------------
  output$hourly_plot <- renderPlot({
    
    start_hour <- hour(input$hour_range[1])
    end_hour   <- hour(input$hour_range[2])
    
    if (end_hour < start_hour) {
      tmp <- start_hour
      start_hour <- end_hour
      end_hour   <- tmp
    }
    
    if (input$hour_metric == "Steps") {
      df <- hourly_steps %>%
        mutate(Hour = hour(ActivityHour)) %>%
        filter(Hour >= start_hour, Hour <= end_hour) %>%
        group_by(Hour) %>%
        summarise(Average = mean(StepTotal, na.rm = TRUE), .groups = "drop")
      
      y_lab  <- "Average Steps"
      title_ <- "Average Steps by Hour of Day"
      
    } else {
      df <- hourly_cal %>%
        mutate(Hour = hour(ActivityHour)) %>%
        filter(Hour >= start_hour, Hour <= end_hour) %>%
        group_by(Hour) %>%
        summarise(Average = mean(Calories, na.rm = TRUE), .groups = "drop")
      
      y_lab  <- "Average Calories"
      title_ <- "Average Calories by Hour of Day"
    }
    
    ggplot(df, aes(x = Hour, y = Average)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.3) +
      scale_x_continuous(
        breaks = 0:23,
        labels = hour_labels
      ) +
      labs(
        title = title_,
        x = "Hour of Day",
        y = y_lab
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ----------------------------------------------------------
  # Weekday vs Weekend Activity
  # ----------------------------------------------------------
  output$weekday_plot <- renderPlot({
    
    ggplot(weekday_hourly_steps,
           aes(x = Hour, y = AvgSteps, color = DayType)) +
      geom_line(linewidth = 1.2) +
      scale_x_continuous(
        breaks = 0:23,
        labels = hour_labels
      ) +
      labs(
        x = "Hour of Day",
        y = "Average Steps",
        color = "Day Type"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ----------------------------------------------------------
  # Sleep Quality Dashboard
  # ----------------------------------------------------------
  output$sleep_quality_plot <- renderPlot({
    
    ggplot(sleep_quality_data,
           aes(x = HoursInBed, y = HoursAsleep)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(
        x = "Hours in Bed",
        y = "Hours Asleep"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Sleep Consistency Calendar
  # ----------------------------------------------------------
  output$sleep_calendar_plot <- renderPlot({
    
    uid <- as.numeric(input$sleep_user_select)
    
    data <- sleep_calendar_all %>%
      filter(Id == uid)
    
    validate(
      need(nrow(data) > 0, "No sleep data for this user.")
    )
    
    ggplot(data, aes(x = Week, y = Weekday, fill = HoursAsleep)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#deebf7", high = "#08519c") +
      labs(
        x = "Week of Year",
        y = "",
        fill = "Hours Asleep"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----------------------------------------------------------
  # User map with bubble size/color
  # ----------------------------------------------------------
  output$user_map <- renderLeaflet({
    
    df <- user_map_data %>%
      filter(mean_steps >= input$min_steps)
    
    validate(
      need(nrow(df) > 0, "No users meet this filter. Try lowering the minimum steps.")
    )
    
    metric_values <- if (input$map_metric == "steps") df$mean_steps else df$mean_calories
    metric_title  <- if (input$map_metric == "steps") "Average Daily Steps" else "Average Daily Calories"
    
    pal <- colorNumeric("Blues", domain = metric_values)
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~pmax(4, mean_steps / max(mean_steps, na.rm = TRUE) * 15),
        color = ~pal(metric_values),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0(
          "<b>User ID:</b> ", Id,
          "<br><b>Avg Steps:</b> ", round(mean_steps),
          "<br><b>Avg Calories:</b> ", round(mean_calories)
        )
      ) %>%
      addLegend(
        "bottomright",
        pal     = pal,
        values  = metric_values,
        title   = metric_title
      )
  })
  
}

# ============================================================
# Run app
# ============================================================
shinyApp(ui, server)
