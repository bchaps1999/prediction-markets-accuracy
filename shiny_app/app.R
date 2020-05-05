

library(shiny)
library(shinythemes)
library(gt)
library(ggpubr)
library(plotrix)
library(tidymodels)
library(tidyverse)

# Import master data file

dem_primary <- read_csv("dem_primary.csv")

# Import data filtered for prediction accuracy over time

predictions_over_time <- read_csv("predictions_over_time.csv")

# Create final results for day before each election

final_results <- dem_primary %>%
    filter(date == contest_date - 1)

# Determine predictions and prediction and accuracy for each variable

predictions <- final_results %>%
    group_by(state, year) %>%
    arrange(desc(market_close)) %>%
    mutate(
        market_rank = 1:n(),
        market_winner = ifelse(market_rank == 1, 1, 0),
        market_correct = ifelse(market_winner == winner, 1, 0)
    ) %>%
    arrange(desc(poll_estimate)) %>%
    mutate(
        poll_rank = 1:n(),
        poll_winner = ifelse(poll_rank == 1, 1, 0),
        poll_correct = ifelse(poll_winner == winner, 1, 0)
    ) %>%
    ungroup() %>%
    filter(winner == 1) %>%
    select(poll_correct, market_correct)

# Create logistic regression model

logistic_mod <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

# List of states for drop down

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
ui <- navbarPage(
    theme = shinytheme("flatly"),
    "How Accurate Are Prediction Markets?",
    tabPanel("State Data",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         "stateInput",
                         "State",
                         choices = state_list,
                         selected = "Iowa"
                     ),
                     selectInput(
                         "yearInput",
                         "Year",
                         choices = c(2016, 2020),
                         selected = "2020"
                     ),
                     p(
                         "Select a state and election year to see how candidates' polling averages and market prices changed over time. Candidates are only displayed for a given state if they did not drop out before the date of the election."
                     )
                 ),
                 mainPanel(plotOutput("statePlot"))
             )),
    
    tabPanel(
        "Market and Poll Data",
        sidebarLayout(sidebarPanel(
            selectInput(
                "plotInput",
                "Prediction Type",
                choices = c("Polling Average", "Prediction Market")
            ),
            p(
                "Select a prediction type to view the data for that variable. Every point on the plot represents a candidate in a specific state and election year. The predictor variables, either final market price or final polling average, are plotted against the vote percent in the election for each candidate. The final values are taken from the day before the election."
            ),
            p(
                "Regression lines are also generated for each plot. A linear regression model was used for the polling average data, while a third-degree polynomial regression was used for market data to create a better fit. The polling average data displays a higher R squared value than the market price data, indicating that the polling average data fits its model more closely than the prediction market."
            )
        ),
        mainPanel(plotOutput("poll_market_plot")))
    ),
    
    tabPanel(
        "Prediction Accuracy",
        column(6,
               wellPanel(
                   h3("Predicting the Election Winner"),
                   gt_output("prediction_table"),
                   br(),
                   p(
                       "To determine how accurate the two variables were at predicting election results, new variables were created to represent whether or not the markets or polls predicted that a specific candidate would win. These variables were then compared to the actual election outcomes to determine a percent accuracy. As can be seen in the table, the prediction markets were seven percentage points more accurate than the polling averages. In total, the markets correctly predicted 4 more elections than the polls. "
                   ),
                   p(
                       "It is important to note, however, that these predictions are based on the final market prices and polling averages from the day before the election. These levels of accuracy likely change over time leading up to the election."
                   )
               )),
        column(6,
               wellPanel(
                   h3("Predicting Vote Percent"),
                   gt_output("error_table"),
                   br(),
                   p(
                       "Although the main purpose of the project is to examine the accuracy of predictions for election outcomes, it is also interesting to consider how well each variable predicts vote percent. To calculate how useful the two variables were for predicting vote percent in the election, the error between predictor variables and vote percent was calculated for each candidate. The polling average was significantly better at predicting the vote percent, as would be expected given the strong linear relationship shown on the Market and Poll Data page."
                   )
               ))
    ),
    
    tabPanel(
        "Modeling Winning",
        column(5,
               wellPanel(
                   h3("Using Logistic Regression to Model Winning"),
                   p(
                       "A logistic regression model can be used to calculate the probability of a candidate winning an election based on their prediction market price. In the model visualized here, market price is the predictor variable for the election outcome variable, which was recorded as a 1 if a candidate won and a 0 if they lost. The resulting model can be used to predict a candidate's chances of winning the election based on their final prediction market price before the election. Any probability greater than 0.50 is considered a predicted win."
                   ),
                   p(
                       "Although regression models can sometimes be used to make more accurate predictions, when this model is applied to the original data set, it is no more accurate at predicting election winners than the market price by itself. This makes sense considering how close the predicted winner threshold is to the 0.50 market price. Adding additional predictor variables, such as polling average, did not increase the accuracy of the model, and some even decreased the accuracy."
                   )
               )),
        column(7,
               plotOutput("winning_plot"))
    ),
    
    tabPanel(
        "Predictions Over Time",
        column(5,
               wellPanel(
                   h3("Changes in Prediction Accuracy Over Time"),
                   p(
                       "To evaluate changes in prediction accuracy over time, a subset of the data was created that had sufficient poll and market data for all thirty days preceding each election. Since not all elections had this necessary data, only 42 elections were used to evaluate changes in prediction accuracy over time."
                   ),
                   p(
                       "As can be seen in the plot, the polls were actually more accurate than the markets until the week before each election. As a result, the polls have a higher average accuracy for predicting election outcomes for the month before an election, but the prediction markets quickly increased in accuracy as the election dates approached."
                   ),
                   p(
                       "The higher accuracy of the prediction markets relative to the polling averages in the week before each election is likely due to the fact that market prices can more quickly respond to updates. Significant news in an election, such as a last minute endorsement or candidates dropping out, is factored into market price more quickly than polls, which take time to carry out and release."
                   )
               )),
        column(
            7,
            plotOutput("time_plot"),
            br(),
            gt_output("average_accuracy")
        )
    ),
    
    tabPanel("About",
             mainPanel(wellPanel(
                 h3("About This Project"),
                 p(
                     "In recent years, prediction markets like PredictIt.org have become popular ways of betting on the outcomes of political events. These markets allow you to buy a contract related to a specific possible outcome, and if that outcome actually happens, you will get a dollar. For example, you could buy a contract for __ cents that you think ______ will win an election, and if they actually do, the website will pay you a dollar. As more people want to buy contracts for a specific outcome that they see as more likely, the higher the price gets, making the market price a valuable prediction tool."
                 ),
                 p(
                     "Some studies have suggested that these markets, which often have hundreds of thousands of contracts sold per day, may be better predictors of political outcomes than actual polls. The purpose of this project is to examine the predictive capacity of prediction markets as compared with polls in the context of the 2016 and 2020 Democratic primary elections."
                 ),
                 p(
                     "Polling data will come from FiveThirtyEight's estimated polling average for each candidate. Market data will come from PredictIt.org, and the daily closing market price will be used as the default value. Election results come from the New York Times and various state websites. Candidates were included in the data set only if they had sufficient market, polling, and election results data for a specific state. States were only included if there was more than one remaining candidate with enough data. As a result, only 59 primaries and caucuses from the two election years are included in the data."
                 ),
                 h3("About Me"),
                 p(
                     "My name is Brendan Chapuis, and I am a sophomore at Harvard College studying government and data science with a minor in economics. You can reach me at chapuis@college.harvard.edu ."
                 )
             )))
)

server <- function(input, output) {
    # Create interactive state plot
    
    output$statePlot <- renderPlot({
        state_data <- dem_primary %>%
            pivot_longer(
                cols = c(poll_estimate, market_close),
                names_to = "prediction_type",
                values_to = "value"
            ) %>%
            filter(
                year == input$yearInput,
                state == input$stateInput,
                date_dropped >= contest_date |
                    is.na(date_dropped),
                date < contest_date,
                name != "Tulsi Gabbard" &
                    name != "Deval Patrick"
            )
        validate(
            need(
                !is.na(state_data),
                "Error: Insufficient data for this state/year combination. Please try different values."
            )
        )
        state_data %>%
            ggplot(aes(
                x = date,
                y = value,
                color = prediction_type
            )) +
            geom_line() +
            facet_wrap( ~ name) +
            theme_gray() +
            labs(title = "Closing Prediction Market Price and Polling Average",
                 x = "Date",
                 y = "Polling Average / Prediction Market Price") +
            scale_color_discrete(
                name = "Prediction Type",
                labels = c("Prediction Market Price",
                           "Polling Average")
            )
    })
    
    # Create market/poll plot using all data
    
    output$poll_market_plot <- renderPlot({
        if (input$plotInput == "Prediction Market") {
            final_results %>%
                mutate(winner = ifelse(winner == 1, "Yes", "No")) %>%
                ggplot(aes(x = market_close, y = vote_percent)) +
                geom_point(aes(color = winner), alpha = 0.75) +
                stat_smooth(method = "lm",
                            formula = y ~ poly(x, 3, raw = TRUE)) +
                theme_minimal() +
                theme(legend.position = "bottom") +
                labs(
                    title = "Relationship Between Final Prediction Market Price and Vote Percent",
                    subtitle = "for Candidates in 2020 Democratic Primaries and Caucuses",
                    x = "Final Closing Market Price on PredictIt",
                    y = "Percent of Vote in Election",
                    color = "Election Winner"
                ) +
                stat_regline_equation(aes(label =  paste(
                    ..eq.label.., ..rr.label.., sep = "~~~~"
                )),
                formula = y ~ poly(x, 3, raw = TRUE))
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
                stat_regline_equation(aes(label =  paste(
                    ..eq.label.., ..rr.label.., sep = "~~~~"
                )))
            
        }
        
    })
    
    # Create logistic regression plot for visualizing winning
    
    output$winning_plot <- renderPlot({
        final_results %>%
            ggplot(aes(x = market_close, y = winner)) +
            geom_point(alpha = 0.3) +
            theme_grey() +
            stat_smooth(
                method = "glm",
                method.args = list(family = "binomial"),
                se = FALSE
            ) +
            labs(
                title = "Probability of Winning an Election Based on Market Price",
                subtitle = "Using a Logistic Regression Model",
                x = "Final Prediction Market Price",
                y = "Probability of Winning"
            )
    })
    
    # Create table for accuracy of final winner predictions
    
    output$prediction_table <- render_gt(
        predictions %>%
            summarize(
                poll_accuracy = paste(round(sum(poll_correct) / n() * 100, 2), "%", sep = ""),
                market_accuracy = paste(round(sum(market_correct) / n() * 100, 2), "%", sep = "")
            ) %>%
            gt() %>%
            tab_header(title = "Percent Accuracy for Predicting Winner") %>%
            cols_label(poll_accuracy = "Accuracy of Polling Average",
                       market_accuracy = "Accuracy of Prediction Market")
    )
    
    # Create table for accuracy of final vote predictions
    
    output$error_table <- render_gt(
        final_results %>%
            mutate(
                market_error = abs(market_close - vote_percent),
                poll_error = abs(poll_estimate - vote_percent)
            ) %>%
            filter(!is.na(market_error) | !is.na(poll_error)) %>%
            summarize(
                average_market_error = round(mean(market_error) * 100, 2),
                average_poll_error = round(mean(poll_error) * 100, 2)
            ) %>%
            gt() %>%
            tab_header(title = "Average Percentage Point Error",
                       subtitle = "For Predicting Vote Percent") %>%
            cols_label(
                average_poll_error = "Polling Error",
                average_market_error = "Prediction Market Error"
            )
    )
    
    # Create plot for changes in prediction accuracy over time
    
    output$time_plot <- renderPlot({
        predictions_over_time %>%
            pivot_longer(
                cols = c(market, poll),
                names_to = "prediction_type",
                values_to = "value"
            ) %>%
            ggplot(aes(
                x = days_before,
                y = value,
                color = prediction_type
            )) +
            geom_line() +
            theme_grey() +
            scale_x_reverse() +
            labs(
                title = "Changes in Prediction Accuracy Over Time",
                subtitle = "Calculated As Percent of Elections Correctly Predicted",
                x = "Days Before Election",
                y = "Accuracy of Predictions",
                color = "Prediction Type"
            ) +
            scale_color_discrete(labels = c("Prediction Market Price", "Polling Average")) +
            theme(legend.position = "bottom")
    })
    
    # Create table for average accuracy of predictions
    
    output$average_accuracy <- render_gt(
        predictions_over_time %>%
            summarize(
                average_market_accuracy = paste(round(mean(market), 2), "%", sep = ""),
                average_poll_accuracy = paste(round(mean(poll), 2), "%", sep = "")
            ) %>%
            gt() %>%
            tab_header(title = "Average Accuracy of Predictions",
                       subtitle = "For 30 Days Preceding Election") %>%
            cols_label(
                average_market_accuracy = "Average Market Accuracy",
                average_poll_accuracy = "Average Poll Accuracy"
            )
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)
