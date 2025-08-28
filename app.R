library(tidyverse)
library(shiny)
library(fpp3)
library(urca)
library(here)
library(gt)
library(markdown)
library(patchwork)  

# Load and prepare data
aus_wine <- read_csv(here("data", "AustralianWines.csv"), na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |> 
    fill(Rose, .direction = "down") |> 
    mutate(Month = yearmonth(mdy(str_replace(Month, '-', '-01-')))) |>
    pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |> 
    as_tsibble(index = Month, key = Varietal)

# Get data ranges and choices
varietal_choices <- unique(aus_wine$Varietal)
available_years <- year(aus_wine$Month) |> unique() |> sort()
month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ui <- fluidPage(
    titlePanel("Australian Wine Sales Analysis"),
    
    tabsetPanel(
        # Visualize Tab
        tabPanel("Visualize",
            fluidRow(
                column(4,
                    selectInput("varietals", "Select Varietals:",
                               choices = varietal_choices,
                               selected = varietal_choices,
                               multiple = TRUE),
                    
                    selectInput("start_year", "Start Year:",
                               choices = available_years,
                               selected = min(available_years)),
                    
                    selectInput("start_month", "Start Month:",
                               choices = setNames(1:12, month_abbr),
                               selected = 1),
                    
                    selectInput("end_year", "End Year:",
                               choices = available_years,
                               selected = max(available_years)),
                    
                    selectInput("end_month", "End Month:",
                               choices = setNames(1:12, month_abbr),
                               selected = 12),
                    
                    checkboxInput("free_y", "Free Y-axis scales", value = TRUE)
                ),
                
                column(8,
                    plotOutput("time_series_plot", height = "500px")
                )
            )
        ),
        
        # Model Tab
        tabPanel("Model",
            fluidRow(
                column(12,
                    h4("Time Series Models"),
                    p("Uses the date range from Visualize tab as training data, with 12-month forecast horizon."),
                    
                    fluidRow(
                        column(3,
                            selectInput("metric_choice", "Select Metric:",
                                       choices = c("RMSE", "MAPE"),
                                       selected = "RMSE")
                        ),
                        column(3,
                            selectInput("sort_varietal", "Sort by Varietal:",
                                       choices = NULL,  # Will be updated dynamically
                                       selected = NULL)
                        ),
                        column(3,
                            checkboxInput("show_training", "Show Training Metrics", value = TRUE)
                        ),
                        column(3,
                            checkboxInput("show_specs", "Show Model Specifications", value = FALSE)
                        )
                    ),
                    
                    conditionalPanel(
                        condition = "input.show_specs",
                        h5("Model Specifications:"),
                        verbatimTextOutput("specs_table"),
                        br()
                    ),
                    
                    conditionalPanel(
                        condition = "input.show_training",
                        br(),
                        h5("Training Period Metrics:"),
                        textOutput("training_dates"),
                        htmlOutput("train_table")
                    ),
                    
                    br(),
                    h5("Test Period Metrics (12-month forecast):"),
                    textOutput("forecast_dates"),
                    htmlOutput("test_table")
                )
            )
        ),
        
        # Forecast Tab
        tabPanel("Forecast",
            fluidRow(
                column(12,
                    h4("12-Month Forecasts"),
                    p("Forecasts based on models fitted to the date range from Visualize tab."),
                    
                    fluidRow(
                        column(6,
                            numericInput("confidence_level", "Confidence Level (%):",
                                        value = 95, min = 50, max = 99, step = 1)
                        ),
                        column(6,
                            numericInput("years_display", "Years of Actual Data to Display:",
                                        value = 2, min = 1, max = 10, step = 1)
                        )
                    ),
                    
                    plotOutput("forecast_plot", height = "600px")
                )
            )
        ),
        
        # About Tab
        tabPanel("About",
            fluidRow(
                column(12,
                    div(
                        includeMarkdown("about.md"),
                        style = "margin-top: 20px;"
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive data filtering for visualization
    filtered_data <- reactive({
        req(input$varietals, input$start_year, input$end_year, 
            input$start_month, input$end_month)
        
        start_date <- yearmonth(paste(input$start_year, input$start_month, "01", sep = "-"))
        end_date <- yearmonth(paste(input$end_year, input$end_month, "01", sep = "-"))
        
        aus_wine |>
            filter(Varietal %in% input$varietals,
                   Month >= start_date,
                   Month <= end_date) 
        
    })
    
    # Create total of selected varietals
    total_data <- reactive({
        data <- filtered_data()
        req(nrow(data) > 0)
        
        data |>
            index_by(Month) |>  # Use index_by() instead of group_by() for tsibbles
            summarize(Sales = sum(Sales, na.rm = TRUE), .groups = "drop") |>
            mutate(Varietal = "Total of Selected") |>
            as_tibble()  # Convert back to regular tibble for plotting
    })
    
    # Time series plot
    output$time_series_plot <- renderPlot({
        data <- filtered_data()
        total <- total_data()
        req(nrow(data) > 0, nrow(total) > 0)
        
        # Main faceted plot
        p1 <- data |>
            ggplot(aes(x = Month, y = Sales)) +
            geom_line() +
            facet_wrap(~ Varietal, scales = if(input$free_y) "free_y" else "fixed") +
            labs(title = "Australian Wine Sales by Varietal",
                 x = "Month",
                 y = "Sales (thousands of liters)") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Total plot
        p2 <- total |>
            ggplot(aes(x = Month, y = Sales)) +
            geom_line(color = "blue", linewidth = 1) +
            geom_smooth(method = "loess", se = TRUE, color = "red", alpha = 0.3) +
            labs(title = "Total of Selected Varietals",
                 x = "Month",
                 y = "Sales (thousands of liters)") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Combine plots vertically using patchwork
        p1 / p2 + plot_layout(heights = c(3, 1))
    }, height = 650)  # Increase height to accommodate both plots
    
    # Update sort varietal choices based on selected varietals
    observe({
        data <- filtered_data()
        if (nrow(data) > 0) {
            varietals <- unique(data$Varietal)
            updateSelectInput(session, "sort_varietal", 
                            choices = varietals,
                            selected = varietals[1])
        }
    })
    
    # Model results (reactive to filtered data from Visualize tab)
    model_results <- eventReactive(filtered_data(), {
        data <- filtered_data()
        req(nrow(data) > 0)
        
        # Check if we have sufficient data
        total_months <- nrow(data) / length(unique(data$Varietal))
        if (total_months < 24) {
            return(list(
                error = "Error: Insufficient data for modeling. Please select a date range with at least 24 months to allow for 12-month test period.",
                models = NULL
            ))
        }
        
        # Create training data (all but last 12 months for each varietal)
        training_data <- data |>
            group_by(Varietal) |>
            arrange(Month) |>
            slice_head(n = -12) |>
            ungroup() |>
            as_tsibble(index = Month, key = Varietal)
        
        # Build models
        models <- training_data |>
            model(
                ETS = ETS(Sales),
                ARIMA = ARIMA(Sales),
                TSLM = TSLM(Sales ~ trend() + season()),
                baseline = decomposition_model(
                    STL(log(Sales) ~ season(window = "periodic")),
                    SNAIVE(season_year),
                    TSLM(season_adjust ~ trend())
                )
            ) |>
            mutate(combo = (ETS + ARIMA + TSLM + baseline) / 4)
        
        # Generate forecasts
        forecasts <- models |> forecast(h = 12)
        
        # Training accuracy
        training_accuracy <- models |> accuracy()
        
        # Test accuracy
        test_accuracy <- forecasts |> accuracy(data)
        
        list(
            error = NULL,
            models = models,
            training_accuracy = training_accuracy,
            test_accuracy = test_accuracy
        )
    })
    
    # Training dates display
    output$training_dates <- renderText({
        results <- model_results()
        
        if (!is.null(results$error)) {
            return("")
        } else {
            data <- filtered_data()
            training_data <- data |>
                group_by(Varietal) |>
                arrange(Month) |>
                slice_head(n = -12) |>
                ungroup()
            
            train_start <- min(training_data$Month)
            train_end <- max(training_data$Month)
            
            paste("Training Period:", format(train_start, "%b %Y"), "to", format(train_end, "%b %Y"))
        }
    })
    
    # Forecast dates display  
    output$forecast_dates <- renderText({
        results <- model_results()
        
        if (!is.null(results$error)) {
            return("")
        } else {
            data <- filtered_data()
            test_data <- data |>
                group_by(Varietal) |>
                arrange(Month) |>
                slice_tail(n = 12) |>
                ungroup()
            
            forecast_start <- min(test_data$Month)
            forecast_end <- max(test_data$Month)
            
            paste("Forecast Horizon:", format(forecast_start, "%b %Y"), "to", format(forecast_end, "%b %Y"))
        }
    })
    
    # Model specifications table
    output$specs_table <- renderPrint({
        results <- model_results()
        
        if (!is.null(results$error)) {
            cat(results$error)
        } else {
            cat("=== MODEL SPECIFICATIONS ===\n\n")
            print(results$models |> t())
        }
    })
    
    # Training metrics table
    output$train_table <- renderUI({
        results <- model_results()
        
        if (!is.null(results$error) || !input$show_training) {
            return(NULL)
        } else {
            # Pivot and sort by selected varietal
            table_data <- results$training_accuracy |>
                select(Varietal, .model, all_of(input$metric_choice)) |>
                pivot_wider(names_from = Varietal, values_from = all_of(input$metric_choice)) |>
                rename(Model = .model)
            
            # Sort by selected varietal column if it exists
            if (!is.null(input$sort_varietal) && input$sort_varietal %in% names(table_data)) {
                table_data <- table_data |>
                    arrange(!!sym(input$sort_varietal))
            }
            
            table_data |>
                gt() |>
                tab_header(title = paste("Training Period Performance -", input$metric_choice)) |>
                fmt_number(
                    columns = -Model,
                    decimals = 3
                ) |>
                data_color(
                    columns = -Model,
                    colors = scales::col_numeric("RdYlGn", domain = NULL, reverse = TRUE)
                ) |>
                as_raw_html()
        }
    })
    
    # Test metrics table
    output$test_table <- renderUI({
        results <- model_results()
        
        if (!is.null(results$error)) {
            tibble(Error = results$error) |>
                gt() |>
                tab_header(title = "Error") |>
                cols_width(Error ~ px(600)) |>
                as_raw_html()
        } else {
            # Pivot and sort by selected varietal
            table_data <- results$test_accuracy |>
                select(Varietal, .model, all_of(input$metric_choice)) |>
                pivot_wider(names_from = Varietal, values_from = all_of(input$metric_choice)) |>
                rename(Model = .model)
            
            # Sort by selected varietal column if it exists
            if (!is.null(input$sort_varietal) && input$sort_varietal %in% names(table_data)) {
                table_data <- table_data |>
                    arrange(!!sym(input$sort_varietal))
            }
            
            table_data |>
                gt() |>
                tab_header(title = paste("Test Period Performance (12-month Forecast) -", input$metric_choice)) |>
                fmt_number(
                    columns = -Model,
                    decimals = 3
                ) |>
                data_color(
                    columns = -Model,
                    colors = scales::col_numeric("RdYlGn", domain = NULL, reverse = TRUE)
                ) |>
                tab_footnote(
                    footnote = "Lower values indicate better performance. Colors: Green = Better, Red = Worse",
                    locations = cells_column_labels(columns = -Model)
                ) |>
                as_raw_html()
        }
    })
    
    # Forecast plot
    output$forecast_plot <- renderPlot({
        results <- model_results()
        
        if (!is.null(results$error)) {
            # Create empty plot with error message
            ggplot() + 
                annotate("text", x = 0.5, y = 0.5, label = results$error, 
                        size = 5, hjust = 0.5, vjust = 0.5) +
                xlim(0, 1) + ylim(0, 1) +
                theme_void()
        } else {
            data <- filtered_data()
            n_varietals <- length(unique(data$Varietal))
            
            # Generate forecasts from the models
            forecasts <- results$models |> forecast(h = 12)
            
            # Trim data for display based on selected years
            display_data <- data |>
                group_by(Varietal) |>
                slice_tail(n = 12 * input$years_display) |>
                ungroup()
            
            # Create the forecast plot faceted by model and varietal
            forecasts |>
                autoplot(level = input$confidence_level) +
                facet_grid(.model ~ Varietal, scales = "free_y") +
                autolayer(display_data, Sales, color = "black") +
                labs(
                    title = paste("12-Month Forecasts with", input$confidence_level, "% Confidence Intervals"),
                    x = "Month",
                    y = "Sales (thousands of liters)"
                ) +
                guides(color = "none") +
                theme(
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none"
                )
        }
    })
}

shinyApp(ui = ui, server = server)