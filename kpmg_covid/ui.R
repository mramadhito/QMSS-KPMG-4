library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19 in US", id="nav",
             
             tabPanel("Nationwide",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                          span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
                          # Input: Selector for date ----
                          
                          dateInput(inputId = "date", label = "Date:", value = "2020-12-02",
                                      min="2020-01-22",
                                      max="2020-12-02"),
                          
                          pickerInput("case_select", "Case Indicator:",   
                                      choices = c("new_cases","new_cases_7","new_cases_14"), 
                                      selected = c("new_cases","new_cases_7","new_cases_14"),
                                      multiple = TRUE),
                          pickerInput("death_select", "Death Indicator:",   
                                      choices = c("new_deaths","new_deaths_7","new_deaths_14"), 
                                      selected = c("new_deaths","new_deaths_7","new_deaths_14"),
                                      multiple = TRUE),
                          DT::dataTableOutput("mytable")
                         ),
                        
                        mainPanel(
                          plotOutput(outputId = "map_plot"),
                          
                          tabsetPanel(
                            tabPanel("Cumulative", plotOutput(outputId = "cumulative_plot")),
                            tabPanel("New Cases", plotOutput(outputId = "case_plot")),
                            tabPanel("New Deaths", plotOutput(outputId = "death_plot"))
              
                          )
                        )
                      )
             ),
             
             tabPanel("State",
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "state",
                                      label = "Choose a state:",
                                      choices = state_name),
                          
                          sliderInput("date2",
                                      "Date:",
                                      min = as.Date("2020-01-22","%Y-%m-%d"),
                                      max = as.Date("2020-12-02","%Y-%m-%d"),
                                      value=as.Date("2020-12-01"),
                                      timeFormat="%d %b"),
                          
                          pickerInput("indicator_2", "Indicator:",   
                                      choices = c("New Confirmed Cases", "New Deaths"), 
                                      selected = c("New Confirmed Cases"),
                                      multiple = FALSE),
  
                          
                          plotOutput(outputId = "box_plot"),
                          
                          h4(textOutput("summary"), align = "right"),
                          ),
                        
                        mainPanel(
                          # Output: New Confirmed Cases ----
                          
                            plotOutput(outputId = "mob_plot"),
                           plotOutput(outputId = "policy_plot")
                          
                        )
                      )
             )
             
             
  )          
)
