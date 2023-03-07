library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "About",
      h1("General Information"),
      p("This dataset includes temperature at specific", em("time and region.")),
      br(),
      p("Number of rows of the dataset:"),
      uiOutput("row"),
      p("Number of columns of the dataset:"),
      uiOutput("col"),
      br(),
      p("The temperature data is measured in degree Celsius and the dataset includes data from 1978-2023."),
      h2("Sample Data"),
      dataTableOutput("sample"),
      verbatimTextOutput("information")
    ),
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          h3("Plot Setting"),
          p("You can see the temperature data for the selected region."),
          p("You see a yearly scatterplot and the corresponding trend lines.\n"),
          p("You can also change the color of the data points by selecting the color."),
          checkboxInput(
            "trend",
            "Display Trend Line"
          ),
          radioButtons(
            "color",
            "Color of the points",
            c("blue", "red", "purple", "black")
          ),
          uiOutput("select")
        ),
        mainPanel(
          plotOutput("plot"),
          textOutput("text_output")
        )
      )
    ),
    tabPanel(
      "Table",
      sidebarLayout(
        sidebarPanel(
          h3("Table"),
          p("This panel displays average temperature over months or years"),
          radioButtons(
            "avg_filter",
            "Average over:",
            c("Month", "Year")
          )
        ),
        mainPanel(
          verbatimTextOutput("range"),
          h2("Average Temperature"),
          tableOutput("table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Render the table
  UAH_data <- read_delim("UAH-lower-troposphere-long.csv.bz2")
  
  UAH_sample <- sample_n(UAH_data, 5)
  
  output$select <- renderUI({
    selectInput(
      "region_select",
      "Choose the region to display",
      choices = UAH_data[["region"]],
      selected = "global"
    )
  })
  
  plot_data <- reactive({
    UAH_data %>% 
      group_by(year, region) %>% 
      mutate(avg_temp = mean(temp)) %>% 
      filter(region == input$region_select)
  })
  
  avg <- reactive({
    avgUAH_data %>% 
      group_by(year, region) %>% 
      mutate(re_avg = mean(avg_temp))
  })
  
  table_data <- reactive({
    if (input$avg_filter == "Month"){
      filtered <- UAH_data %>% 
        group_by(year, month) %>% 
        summarise(avg = mean(temp))
    }else if (input$avg_filter == "Year"){
      filtered <- UAH_data %>% 
        group_by(year) %>% 
        summarise(avg = mean(temp))
    }
    return(filtered)
  })
  
  output$table <- renderTable({
    UAH_data
  })
  
  output$information <- renderText({
    "This dataset is retrieved from UAH"
  })
  
  output$row <- renderUI({
    strong(paste(nrow(UAH_data)))
  })
  
  output$col <- renderUI({
    strong(paste(ncol(UAH_data)))
  })
  
  output$sample <- renderDataTable({
    UAH_sample
  })
  
  output$plot <- renderPlot({
    ggplot(data = plot_data(), mapping = aes(x = year, y = avg_temp)) + 
      ggtitle(paste("Average temperature in", input$region_select)) +
      geom_point(color = input$color) + 
      if (input$trend) {
        geom_smooth(method = lm)
      } else {
        geom_blank()
      }
  })
  
  observeEvent(input$region_select, {
    output$text_output <- renderText({
      paste("Selected subset includes", 
            n_distinct(plot_data()), 
            "unique observations.",
            "The maximum temperature in this subset is",
            round(max(plot_data()$avg_temp),3),
            "and the minimum temperature is",
            round(min(plot_data()$avg_temp),3)
            )
    })
  })
  
  output$range <- renderText({
    paste("The range of the table is", 
          round(max(table_data()$avg),3), 
          "and", 
          round(min(table_data()$avg),3)
    )
  })
  
  output$table <- renderTable({
    table_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
