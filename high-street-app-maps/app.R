library(shiny)
library(tidyverse)
library(sf)
library(shinyWidgets)

# data set from code/1_data_cleaning/make_app_data.R
dta_boroughs <- read_csv("london_boroughs.csv")
shp_boroughs <- read_sf("london_boroughs_shp/London_Borough_Excluding_MHW.shp")

months <- unique(dta_boroughs$month_year)
months2 <- sapply(months, function(i) paste(str_split(i,"_")[[1]][2],str_split(i,"_")[[1]][1],sep = "_") )
months <- months[order(months2)]

map_colours <- c("#F0E442","#d1495b")
names(map_colours) <- c("temporarily_closed","permanently_closed")

ui <- fluidPage(
  
  #title = "High Street Closures",
  
  # - - - % % % - - -
  # First Row: Input
  # - - - % % % - - -
  fluidRow(
    column(6, # Choose Topic
           selectInput(
             inputId = "input_main",
             label = "Choose Topic",
             choices = c("Temporary Closures"="temporarily_closed",
                         "Permanent Closures"="permanently_closed"),
             selected = "Temporary Closures"
           )
    ),
    column(6, # Choose Master Category
           selectInput(
             inputId = "master_category",
             label = "Category",
             choices = c("Restaurants"="restaurants",
                         "Shops"="shopping"),
             selected = "shops"
           )
    )
  ),
  fluidRow(
    column(12,
      radioButtons(
        inputId = "month",
        label = "Month",
        choices = months,
        selected = "01_2021",
        inline = TRUE
      )
    )
  ),
  # - - - % % % - - -
  # Second Row: Output
  # - - - % % % - - -
  fluidRow(
           #textOutput("print_cities"),
           plotOutput("map_plot")
  )
)

server <- function(input, output, session) {
  
  dta_map <- reactive({
    
      shp_boroughs %>%
      left_join(dta_boroughs %>%
                    filter(master_category == input$master_category,
                           month_year == input$month),
                  by=c("NAME" = "borough")) %>%
      mutate(`Fraction Closed` = !!as.symbol(input$input_main))
    
  })
  
  output$map_plot <- renderPlot({
    ggplot() + geom_sf(data=dta_map(),
                       aes(geometry = geometry,
                           colour = `Fraction Closed`,
                           fill = `Fraction Closed`)
                       ) +
      xlab("") + ylab("") +
      scale_fill_gradient(low="#66a182",high=map_colours[[input$input_main]]) +
      scale_colour_gradient(low="#66a182",high=map_colours[[input$input_main]])
    
  })
}

shinyApp(ui, server)
