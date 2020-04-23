
library(shiny)
library(shinythemes)
library(tidyverse)

dem_primary <- read_csv("dem_primary.csv") %>% 
    select(-X1)

final_results_data <- read_csv("final_results_data.csv") %>% 
    select(-X1)

state_list <- c(
    "Alabama",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Florida",
    "Idaho",
    "Illinois",
    "Iowa",
    "Maine",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Nevada",
    "New Hampshire",
    "North Carolina",
    "Oklahoma",
    "South Carolina",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "Wisconsin"
)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
    "Predictive Capacity of Polls and Prediction Markets",
    tabPanel("State Data",
        sidebarLayout(
            sidebarPanel(
                selectInput("stateInput", "State",
                            choices = state_list,
                            selected = "Iowa")
                ),
        mainPanel(
            plotOutput("statePlot")
            )
        )
    ),
    tabPanel("Poll and Market Data",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("plotInput", "Data Source",
                                 choices = c("Polling Average", "Prediction Market"))
                 ),
                 mainPanel(
                     plotOutput("poll_market_plot")
                 )
             )
    ),
    tabPanel("Prediction Models",
             mainPanel("main")
             ),
    tabPanel("About",
             mainPanel(
                 p("In recent years, prediction markets like PredictIt.org have become popular ways of betting on the outcomes of political events. These markets allow you to buy a contract related to a specific possible outcome, and if that outcome actually happens, you will get a dollar. For example, you could buy a contract for __ cents that you think Bernie Sanders will win the democratic primary, and if he actually does, the website will pay you a dollar. As more people buy contracts for a specific outcome, the higher the price gets as it is seen as a more likely possibility.  

Some studies have suggested that these markets, which often have hundreds of thousands of contracts sold per day, may be better predictors of political outcomes than actual polls. The purpose of this project is to examine some of the relationships between data from polls and prediction markets within the context of the 2020 Democratic primary elections. The relationships that will be explored in this project include the correlation between polling results and market prices and the correlation between market prices right before a vote and the results of the vote.

Polling data will come from fivethirtyeight's estimated polling average for each candidate, and will be updated regularly as more polls are released. Market data will come from PredictIt.org. Since market data for each primary state can only be accessed after the results are confirmed, more states will be added over time."
                 )
             )
    )
)

server <- function(input, output) {

    output$statePlot <- renderPlot({
        dem_primary %>% 
            pivot_longer(cols = c(poll_estimate, market_close),
                         names_to = "prediction_type",
                         values_to = "value") %>% 
            filter(state == input$stateInput,
                   date_dropped >= contest_date | is.na(date_dropped),
                   date < contest_date,
                   name != "Tulsi Gabbard" & name != "Deval Patrick") %>% 
            ggplot(aes(x = date, y = value, color = prediction_type)) +
            geom_line() + 
            facet_wrap(~ name) + 
            theme_gray() + 
            labs(title = "Closing Prediction Market Price and Polling Average",
                 x = "Date",
                 y = "Polling Average (%) / Prediction Market Price ($)") +
            scale_color_discrete(name = "Prediction Type",
                                 labels = c("Prediction Market Price",
                                            "Polling Average"))
    })
    
    output$poll_market_plot <- renderPlot({
        if (input$plotInput == "Prediction Market") {
            final_results_data %>%
                mutate(winner = ifelse(winner == 1, "Yes", "No")) %>% 
                ggplot(aes(x = market_close, y = vote_percent)) +
                geom_point(aes(color = winner), alpha = 0.75) +
                stat_smooth(method="lm",
                            formula=y ~ poly(x, 3, raw=TRUE)) +
                theme_minimal() +
                theme(legend.position="bottom") +
                labs(title = "Relationship Between Final Prediction Market Price and Vote Percent",
                     subtitle = "for Candidates in 2020 Democratic Primaries and Caucuses",
                     x = "Final Closing Market Price on PredictIt",
                     y = "Percent of Vote in Election",
                     color = "Election Winner")
        } else {
            final_results_data %>%
                mutate(winner = ifelse(winner == 1, "Yes", "No")) %>% 
                ggplot(aes(x = poll_estimate, y = vote_percent)) +
                geom_point(aes(color = winner), alpha = 0.75) +
                stat_smooth(method="lm", formula = y~x, se=TRUE) +
                theme_minimal() +
                theme(legend.position="bottom") +
                labs(title = "Relationship Between Final Polling Average and Percent of Vote",
                     subtitle = "for Candidates in 2020 Democratic Primaries and Caucuses",
                     x = "Final Polling Average from FiveThirtyEight",
                     y = "Percent of Vote in Election",
                     color = "Election Winner")
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
