
library(shiny)
library(shinythemes)
library(gt)
library(ggpubr)
library(tidyverse)

dem_primary <- read_csv("dem_primary.csv")

final_results <- dem_primary %>% 
    filter(date == contest_date -1)

predictions <- final_results %>%
    group_by(state, year) %>% 
    arrange(desc(market_close)) %>% 
    mutate(market_rank = 1:n(),
           market_winner = ifelse(market_rank == 1, 1, 0),
           market_correct = ifelse(market_winner == winner, 1, 0)) %>%
    arrange(desc(poll_estimate)) %>% 
    mutate(poll_rank = 1:n(),
           poll_winner = ifelse(poll_rank == 1, 1, 0),
           poll_correct = ifelse(poll_winner == winner,1,0)) %>% 
    ungroup() %>% 
    filter(winner == 1) %>% 
    select(poll_correct, market_correct)

state_list <- c(
    "Alabama",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Florida",
    "Georgia",
    "Indiana",
    "Iowa",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oregon",
    "Rhode Island",
    "South Carolina",
    "Tennessee",
    "Texas",
    "Vermont",
    "Virginia",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
    "Predictive Capacity of Polls and Prediction Markets",
    tabPanel("State Data",
        sidebarLayout(
            sidebarPanel(
                selectInput("stateInput", "State",
                            choices = state_list,
                            selected = "Iowa"),
                selectInput("yearInput", "Year",
                            choices = c(2016, 2020),
                            selected = "2020"),
                p("Select a state and election year to see how candidates' polling averages and market prices changed over time. Candidates are only displayed for a given state if they did not drop out before the date of the election. If an error is displayed, that means there is insufficient data for that year/state combination.")
                ),
        mainPanel(
            plotOutput("statePlot")
            )
        )
    ),
    tabPanel("Poll and Market Data",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("plotInput", "Prediction Type",
                                 choices = c("Polling Average", "Prediction Market")),
                     p("Change the prediction type to determine which plot is displayed. Every point represents a candidate in a specific state and election year. The predictor varaible, either market price or polling average, are plotted against the final vote percent in the election for each candidate."),
                     p("Regression lines are also generated for each plot. A linear regression model was used for the polling average data, while a third-degree polynomial regression was used for market data to create a better fit. The polling average data displays a higher R squared value than the market price data, indicating that the polling average data fits its model more closely than the prediction market.")
                 ),
                 mainPanel(
                     plotOutput("poll_market_plot")
                 )
             )
    ),
    tabPanel("Prediction Accuracy",
             column(6,
                 wellPanel(
                 h3("Predicting the Election Winner"),
                 gt_output("prediction_table"),
                 br(),
                 p(
                     "To determine how accurate the two variables were at predicting election results, new variables were created to represent whether or not the markets or polls predicted that a specific candidate would win. As can be seen in the table, the prediction markets were seven percentage points more accurate than the polling averages. In total, the markets correctly predicted 4 more elections than the polls."
                 )
                 )
                 ),
            column(6,
                   wellPanel(
                 h3("Predicting the Vote Percent"),
                 gt_output("error_table"),
                 br(),
                 p(
                     "To calculate how useful the two variables were for predicting vote percent in the election, the difference between each predictor variable and the vote percent was calculated for each candidate. The polling average was significantly better at predicting the vote percent, as would be expected given that highly favored candidates see market prices close at or near $0.99 and it is nearly impossible for a candidate to receive a corresponding vote percent of 99%."
                 )
             )
            )
            ), 
    tabPanel("Modeling Vote Percent",
             sidebarLayout(sidebarPanel(
                 p("This is a test")
             ),
                           mainPanel(
                           ))
             ),
    tabPanel("Modeling Winning",
             sidebarLayout(
                 sidebarPanel(
                     p("Logistic regression can be used to create an even more accurate model for predicting whether or not a candidate will win an election. This type of model can be visualized by plotting the market price on the X axis and a binary election winner variable on the Y axis. Running a logistic regression on these variables produces the line seen on the plot.")
                 ),
                 mainPanel(
                     plotOutput("winning_plot")
                     )
                 )
             ),
    tabPanel("About",
             mainPanel(
                 p("In recent years, prediction markets like PredictIt.org have become popular ways of betting on the outcomes of political events. These markets allow you to buy a contract related to a specific possible outcome, and if that outcome actually happens, you will get a dollar. For example, you could buy a contract for __ cents that you think ______ will win an election, and if they actually do, the website will pay you a dollar. As more people want to buy contracts for a specific outcome that they see as more likely, the higher the price gets, making the market price a valuable prediction tool."),
                 p("Some studies have suggested that these markets, which often have hundreds of thousands of contracts sold per day, may be better predictors of political outcomes than actual polls. The purpose of this project is to examine the predictive capacity of prediction markets as compared with polls within the context of the 2016 and 2020 Democratic primary elections. Polling and market data will be evaluated as predictors of whether or not a candidate will win as well as candidates' vote percent. This information will then be used to create even more effective prediction models."),
                 p("Polling data will come from fivethirtyeight's estimated polling average for each candidate. Market data will come from PredictIt.org, and the daily closing market price will be used as the default value. Specific candidates were included in the data set only if they had available market, polling, and election results data. States were only included if there was more than one remaining candidate with enough data. As a result, only 59 primaries and caucuses from the two election years are included in the data.")
             )
    )
)

server <- function(input, output) {

    output$statePlot <- renderPlot({
        dem_primary %>% 
            pivot_longer(cols = c(poll_estimate, market_close),
                         names_to = "prediction_type",
                         values_to = "value") %>% 
            filter(year == input$yearInput,
                   state == input$stateInput,
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
            final_results %>%
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
                     color = "Election Winner") +
                stat_regline_equation(
                    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                    formula=y ~ poly(x, 3, raw=TRUE)
                )
        } else {
            final_results %>%
                mutate(winner = ifelse(winner == 1, "Yes", "No")) %>%
                ggplot(aes(x = poll_estimate, y = vote_percent)) +
                geom_point(aes(color = winner), alpha = 0.75) +
                stat_smooth(method = "lm",
                            formula = y ~ x,
                            se = TRUE) +
                theme_minimal() +
                theme(legend.position = "bottom") +
                labs(
                    title = "Relationship Between Final Polling Average and Percent of Vote",
                    subtitle = "for Candidates in 2020 Democratic Primaries and Caucuses",
                    x = "Final Polling Average from FiveThirtyEight",
                    y = "Percent of Vote in Election",
                    color = "Election Winner"
                ) +
                stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))
            
        }
        
    })
    
    output$winning_plot <- renderPlot({
        final_results %>%
            ggplot(aes(x = market_close, y = winner)) + 
            geom_point(alpha = 0.3) + 
            theme_grey() + 
            stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + 
            labs(title = "Probability of Winning an Election Based on Market Price",
                 subtitle = "Using a Logistic Regression Model",
                 x = "Market Price on PredictIt.org",
                 y = "Probability of Winning")
    })
    
    output$prediction_table <- render_gt(
        predictions %>% 
        summarize(poll_accuracy = paste(round(sum(poll_correct)/n() * 100, 2), "%", sep = ""),
                  market_accuracy = paste(round(sum(market_correct)/n() * 100, 2), "%", sep = "")) %>% 
        gt() %>% 
        tab_header(title = "Percent Accuracy for Predicting Winner") %>% 
        cols_label(poll_accuracy = "Polling Average",
                   market_accuracy = "Prediction Market Price")
    )
    
    output$error_table <- render_gt(
        final_results %>% 
            mutate(market_error = abs(market_close - vote_percent),
                   poll_error = abs(poll_estimate - vote_percent)) %>%
            filter(!is.na(market_error) | !is.na(poll_error)) %>% 
            summarize(average_market_error = round(mean(market_error) * 100,2),
                      average_poll_error = round(mean(poll_error) * 100,2)) %>% 
            gt() %>% 
            tab_header(title = "Average Percentage Point Error",
                       subtitle = "For Predicting Vote Percent") %>% 
            cols_label(average_poll_error = "Polling Average",
                       average_market_error = "Prediction Market Price")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
