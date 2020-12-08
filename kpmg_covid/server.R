library(shiny)
library(shinyWidgets)

shinyServer(function(input, output, session) {
  stateInput <- reactive({ input$state })
  
  output$map_plot <- renderPlot({
    make_map(date = input$date)
    
  })
  
  
  output$mytable <- DT::renderDataTable({
    make_table(date=input$date)
  })
  
  output$policy_plot <- renderPlot({
    
    state = stateInput()
    policy_new_cases(state=input$state, date=input$date2)
    
  })
  
  output$mob_plot <- renderPlot({
    
    state = stateInput()
    mobility_new_case(state=input$state, date=input$date2)
    
  })
  
  output$case_plot <- renderPlot({
    
    new_lines(date = input$date, indicators=input$case_select)
    
  })
  
  output$death_plot <- renderPlot({
    
    new_lines(date = input$date, indicators=input$death_select)
    
  })
  
  output$cumulative_plot <- renderPlot({
    
    cum_lines(date = input$date)
    
  })
  
  output$box_plot <- renderPlot({
    
    make_box(state = input$state, indicator = input$indicator_2)
    
  })
  
  output$summary <- renderText({
    paste0(box_values(state=input$state, date=input$date2)[1], " cases (Total)     ", box_values(state=input$state, date=input$date2)[2], " deaths (Total)")
  })
  
  
  
  
})


