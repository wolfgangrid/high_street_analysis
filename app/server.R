library(tidyverse)

server <- function(input, output, session) {
  
  output$print_cities <- renderText({
    input$cities_choice
  })
  
  output$perm_closures_plot <- renderPlot({
    ggplot(data = dta_cities %>% filter(city %in% input$cities_choice,
                                        master_category == input$master_category,
                                        status == input$input_main),
           aes(x=month,y=frac_closed,group=city,colour=city)) +
      geom_line() #+
      #ylim(0,0.4)
      
    
  })
}