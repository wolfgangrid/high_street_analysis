library(shiny)
library(tidyverse)

# data set from code/1_data_cleaning/make_app_data.R
dta_cities <- read.csv("dta_app.csv")
cities <- unique(dta_cities$city)
months <- unique(dta_cities$month)

#source("./ui.R")
#source("./server.R")

ui <- fluidPage(
  
  title = "High Street Closures",
  
  # - - - % % % - - -
  # First Row: Input
  # - - - % % % - - -
  fluidRow(
    column(4, # Choose Topic
           selectInput(
             inputId = "input_main",
             label = "Choose Topic",
             choices = c("Temporary Closures"="temporarily_closed",
                         "Permanent Closures"="permanently_closed"),
             selected = "Temporary Closures"
           )
    ),
    column(4, # Choose Master Category
           selectInput(
             inputId = "master_category",
             label = "Category",
             choices = c("Restaurants"="restaurants",
                         "Shops"="shopping"),
             selected = "shops"
           )
    ),
    column(4, # Choose Cities
           #checkboxInput("show_london", "London", value = TRUE),
           #checkboxInput("show_manchester", "Manchester", value = TRUE)
    )
  ),
  # - - - % % % - - -
  # Second Row: Output
  # - - - % % % - - -
  fluidRow(
    column(2,
           checkboxGroupInput("cities_choice", label = NULL,
                              choices = cities,
                              selected = cities, inline = FALSE)
    ),
    column(10,
           #textOutput("print_cities"),
           plotOutput("perm_closures_plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$print_cities <- renderText({
    input$cities_choice
  })
  
  output$perm_closures_plot <- renderPlot({
    ggplot(data = dta_cities %>% filter(city %in% input$cities_choice,
                                        master_category == input$master_category,
                                        status == input$input_main),
           aes(x=month,y=frac_closed,group=city,colour=city)) +
      geom_line() +
      xlab("Month") +
      ylab("") +
      scale_x_continuous(breaks = as.numeric(months)) +
      theme(text = element_text(size=14),
            aspect.ratio=8/10)
    
    
  })
}

shinyApp(ui, server)
