

library(shiny)
library(shinythemes)
library(gt)
library(ggpubr)
library(plotrix)
library(tidymodels)
library(tidyverse)

dem_primary <- read_csv("dem_primary.csv")

final_results <- dem_primary %>%
    filter(date == contest_date - 1)

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

logistic_mod <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

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
    "Predictive Capacity of Polls and Prediction Markets",
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
        "Poll and Market Data",
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
                   p("It is important to note, however, that these predictions are based on the final market prices and polling averages from the day before the election. These levels of accuracy likely change over time leading up to the election.")
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
    tabPanel("Modeling Winning",
             fluidRow(column(1),
                      column(
                          10,
                          wellPanel(
                              h3("Using a Logistic Regression Model"),
                                  plotOutput("winning_plot"),
                              br(),
                                  p(
                                      "A logistic regression model can be used to calculate the probability of a candidate winning an election based on their prediction market price. In the model visualized here, market price is the predictor variable for the election outcome variable, which was recorded as a 1 if a candidate won and a 0 if they lost. The resulting model can be used to predict a candidate's chances of winning the election based on their final prediction market price before the election. Any probability greater than 0.50 is considered a predicted win."
                                  ),
                                  p(
                                      "However, when this model is applied to the original data set, it is no more accurate at predicting election winners than the market price by itself. This makes sense considering how close the predicted winner threshold is to the 0.50 market price. Adding additional predictor variables, such as polling average, did not increase the accuracy of the model, and sometimes even decreased the accuracy."
                                  )
                              # ,
                              # br(),
                              # h3("Making More Accurate Predictions"),
                              # gt_output("winner_table_1"),
                              # br(),
                              # p(
                              #     "This model can then be applied to the original data to predict the election outcomes. The table above shows that this model was five percentage points more accurate at predicting election outcomes than the Prediction Markets by themselves, which corresponds to three more elections predicted correctly. However, since the same elections are being used to create the model and then test it, there is a possibility the results could have been affected by overfitting. To solve this problem, the data can be randomly divided into a training group to create the model and a testing group to evaluate its accuracy. Since the data set is relatively small, it could only realistically be split into one training group and one testing group, so many resamplings were conducted and the accuracies were averaged."
                              # )
                          )
                      ))),
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
                 p("My name is Brendan Chapuis, and I am a sophomore at Harvard College studying government and data science with a minor in economics. You can reach me at chapuis@college.harvard.edu .")
             )))
)

server <- function(input, output) {
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
            need(!is.na(state_data), "Error: Insufficient data for this state/year combination. Please try different values.")
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
    
}

# Run the application
shinyApp(ui = ui, server = server)
