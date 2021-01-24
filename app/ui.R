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