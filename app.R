library(shiny)
library(plotly)
library(jsonlite)
library(DT)

ui <- fluidPage(
  titlePanel("Create Charts"),  # Title of the application
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "Choose chart type:", 
      choices = c("Histogram", "Bar Chart", "Scatter Plot", "Box Plot")),  # Dropdown for selecting chart type
      textInput("chart_title", "Chart Title:"),  # Text input for chart title
      selectInput("x_axis_label", "X-Axis Label:", ""),  # Dropdown for X-axis label
      selectInput("y_axis_label", "Y-Axis Label:", ""),  # Dropdown for Y-axis label
      fileInput("file", "Choose JSON file:")  # File input for JSON file
    ),
    mainPanel(
      plotlyOutput("plot"),  # Output for the plot
      DTOutput("data_table")  # Output for the data table
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$file)
    json_file <- input$file$datapath
    tryCatch(
      {
        data <- fromJSON(json_file)  # Read JSON data from the selected file
        return(data)
      },
      error = function(e) {
        # Handle error if JSON is malformed or cannot be read
        return(NULL)
      }
    )
  })

  observe({
    if (!is.null(input$file) && !is.null(data())) {
      col_names <- colnames(data())
      # Set default values for X and Y axis labels based on column names
      updateSelectInput(session, "x_axis_label", choices = col_names)
      updateSelectInput(session, "y_axis_label", choices = col_names)
    }
  })

  output$plot <- renderPlotly({
    if (!is.null(input$file) && !is.null(data())) {
      x_col <- input$x_axis_label
      y_col <- input$y_axis_label
      if (input$chart_type == "Histogram") {
        p <- plot_ly(data(), x = data()[[x_col]], type = "histogram", 
                     xaxis = list(title = x_col),
                     yaxis = list(title = y_col)) %>%
          layout(title = input$chart_title) %>%
          add_annotations(
            text = x_col,
            x = 0.5,
            y = -0.1,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          ) %>%
          add_annotations(
            text = y_col,
            x = -0.1,
            y = 0.5,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            textangle = -90
          )
      } else if (input$chart_type == "Bar Chart") {
        p <- plot_ly(data(), x = data()[[x_col]], y = data()[[y_col]], type = "bar",
                     xaxis = list(title = x_col),
                     yaxis = list(title = y_col)) %>%
          layout(title = input$chart_title)
      } else if (input$chart_type == "Scatter Plot") {
        p <- plot_ly(data(), x = data()[[x_col]], y = data()[[y_col]], type = "scatter",
                     mode = "markers",
                     xaxis = list(title = x_col),
                     yaxis = list(title = y_col)) %>%
          layout(title = input$chart_title)
      } else if (input$chart_type == "Box Plot") {
        p <- plot_ly(data(), y = data()[[y_col]], type = "box",
                     xaxis = list(title = x_col),
                     yaxis = list(title = y_col)) %>%
          layout(title = input$chart_title)
      }
      return(p)
    }
  })

  output$data_table <- renderDT({
    if (!is.null(input$file) && !is.null(data())) {
      datatable(data())  # Display the data in an interactive table
    }
  })
}

shinyApp(ui, server)
