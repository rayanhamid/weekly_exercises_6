
library(shiny)
library(tidyverse)

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")



ui <- fluidPage(checkboxGroupInput("checkGroup", label = h3("Select States to compare"), 
                                   choices = list("Alabama"="Alabama", "Alaska" ="Alaska", 
                                                  "Arizona" ="Arizona","Arkansas"="Arkansas", 
                                                  "California" ="California", "Colorado"="Colorado", 
                                                  "Connecticut"="Connecticut","Delaware"="Delaware",
                                                  "Florida"="Florida", "Georgia"="Georgia", 
                                                  "Hawaii"="Hawaii","Idaho"="Idaho", "Illinois"="Illinois",
                                                  "Indiana"="Indiana", "Iowa"="Iowa",
                                                  "Kansas"="Kansas", "Kentucky"="Kentucky",
                                                  "Louisiana"="Louisiana", "Maine"="Maine",
                                                  "Maryland"="Maryland", "Massachusetts"="Massachusetts",
                                                  "Michigan"="Michigan", "Minnesota"="Minnesota",
                                                  "Mississippi"="Mississippi", "Missouri"="Missouri",
                                                  "Montana"="Montana", "Nebraska"="Nebraska",
                                                  "Nevada"="Nevada", "New Hampshire"="New Hampshire",
                                                  "New Mexico"="New Mexico","New York"="New York", 
                                                  "North Carolina"="North Carolina", 
                                                  "North Dakota"="North Dakota","Ohio"="Ohio",
                                                  "Oklahoma"="Oklahoma", "Oregon"="Oregon", "Pennsylvania"="Pennsylvania",
                                                  "Rhode Island"="Rhode Island","South Carolina"="South Carolina",
                                                  "South Dakota"="South Dakota", "Tennessee"="Tennessee",
                                                  "Texas"="Texas", "Utah"="Utah",
                                                  "Vermont"="Vermont", "Virginia"="Virginia",
                                                  "Washington"="Washington", "West Virginia"="West Virginia",
                                                  "Wisconsin"="Wisconsin", "Wyoming"="Wyoming"),
                                   selected = 1),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
      covid19 %>% 
      group_by(state, date, cases) %>% 
      filter(cases >=20) %>%
      group_by(state) %>% 
      mutate(min_month_date = min(date)) %>% 
      mutate(num_cases_since_20 = date - min_month_date) %>% 
      filter( state %in% c(input$checkGroup)) %>% 
      ggplot(aes(y = log10(cases), x= num_cases_since_20, color = state)) +
      geom_line()
  })
}
shinyApp(ui = ui, server = server)

