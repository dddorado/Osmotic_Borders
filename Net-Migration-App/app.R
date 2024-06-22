library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load the dataset
imputed_dataset <- read.csv("dataset.csv")

# Ensure the Year column is numeric and handle non-numeric values
imputed_dataset$Year <- as.numeric(as.character(imputed_dataset$Year))
imputed_dataset <- imputed_dataset %>% filter(!is.na(Year))

# UI
ui <- fluidPage(
  titlePanel("Migration Inequality Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("country", "Select Countries", choices = unique(imputed_dataset$Country), selected = "Albania", multiple = TRUE),
      sliderInput("year", "Select Year Range", min = min(imputed_dataset$Year, na.rm = TRUE), max = max(imputed_dataset$Year, na.rm = TRUE), value = c(min(imputed_dataset$Year, na.rm = TRUE), max(imputed_dataset$Year, na.rm = TRUE)), step = 1),
      selectizeInput("year_tab4", "Select Year for Scatter Plot", choices = unique(imputed_dataset$Year), selected = min(imputed_dataset$Year, na.rm = TRUE), multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Net Migration", plotOutput("migrationPlot")),
        tabPanel("Migration vs Gini", 
                 plotOutput("inequalityPlot"), 
                 plotOutput("correlationPlot")),
        tabPanel("Migration vs GDP-PPP", 
                 plotOutput("gdpPlot"), 
                 plotOutput("gdpCorrelationPlot")),
        tabPanel("Scatter Plot & Rankings",
                 plotlyOutput("scatterPlot"),
                 h3("Top 5 and Bottom 5 Rankings"),
                 h4("Net Migration"),
                 tableOutput("top_bottom_migration"),
                 h4("Gini Index"),
                 tableOutput("top_bottom_gini"),
                 h4("GDP PPP"),
                 tableOutput("top_bottom_gdp"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    imputed_dataset %>%
      filter(Country %in% input$country & Year >= input$year[1] & Year <= input$year[2])
  })
  
  output$migrationPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Net_Migration, color = Country)) +
      geom_line() +
      labs(title = "Net Migration Over Time", x = "Year", y = "Net Migration") +
      theme_minimal()
  })
  
  output$inequalityPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Gini, color = Country)) +
      geom_line() +
      labs(title = "Gini Index Over Time", x = "Year", y = "Gini Index") +
      theme_minimal()
  })
  
  output$correlationPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Gini, y = Net_Migration, color = Country)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Net Migration vs Gini Index", x = "Gini Index", y = "Net Migration") +
      theme_minimal()
  })
  
  output$gdpPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = GDP_PPP, color = Country)) +
      geom_line() +
      labs(title = "GDP-PPP Over Time", x = "Year", y = "GDP-PPP") +
      theme_minimal()
  })
  
  output$gdpCorrelationPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = GDP_PPP, y = Net_Migration, color = Country)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Net Migration vs GDP-PPP", x = "GDP-PPP", y = "Net Migration") +
      theme_minimal()
  })
  
  filtered_data_tab4 <- reactive({
    imputed_dataset %>%
      filter(Year %in% input$year_tab4 & (Country %in% input$country | length(input$country) == 0))
  })
  
  output$scatterPlot <- renderPlotly({
    data <- filtered_data_tab4()
    p <- ggplot(data, aes(x = Gini, y = Net_Migration, size = GDP_PPP, text = paste("Country:", Country, "<br>Gini:", Gini, "<br>Net Migration:", Net_Migration, "<br>GDP PPP:", GDP_PPP))) +
      geom_point(alpha = 0.7) +
      labs(title = "Net Migration vs Gini Index with GDP PPP as Size", x = "Gini Index", y = "Net Migration") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  output$top_bottom_migration <- renderTable({
    data <- imputed_dataset %>% filter(Year %in% input$year_tab4)
    top_5 <- data %>% arrange(desc(Net_Migration)) %>% head(5) %>% select(Country, Net_Migration)
    bottom_5 <- data %>% arrange(Net_Migration) %>% head(5) %>% select(Country, Net_Migration)
    rbind(top_5, bottom_5)
  })
  
  output$top_bottom_gini <- renderTable({
    data <- imputed_dataset %>% filter(Year %in% input$year_tab4)
    top_5 <- data %>% arrange(desc(Gini)) %>% head(5) %>% select(Country, Gini)
    bottom_5 <- data %>% arrange(Gini) %>% head(5) %>% select(Country, Gini)
    rbind(top_5, bottom_5)
  })
  
  output$top_bottom_gdp <- renderTable({
    data <- imputed_dataset %>% filter(Year %in% input$year_tab4)
    top_5 <- data %>% arrange(desc(GDP_PPP)) %>% head(5) %>% select(Country, GDP_PPP)
    bottom_5 <- data %>% arrange(GDP_PPP) %>% head(5) %>% select(Country, GDP_PPP)
    rbind(top_5, bottom_5)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
