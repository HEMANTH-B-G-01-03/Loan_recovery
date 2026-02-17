# # library(shiny)
# # library(randomForest)

# # # Load trained model
# # model <- readRDS("../models/loan_default_rf_model.rds")

# # ui <- fluidPage(
  
# #   titlePanel("Loan Default Prediction System"),
  
# #   sidebarLayout(
    
# #     sidebarPanel(
# #       numericInput("age", "Age", value = 30, min = 18),
# #       numericInput("income", "Annual Income", value = 50000),
# #       selectInput("home", "Home Ownership",
# #                   choices = c("RENT", "OWN", "MORTGAGE", "OTHER")),
# #       numericInput("emp_len", "Employment Length (years)", value = 5),
# #       selectInput("intent", "Loan Intent",
# #                   choices = c("PERSONAL", "EDUCATION", "MEDICAL",
# #                               "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")),
# #       selectInput("grade", "Loan Grade",
# #                   choices = c("A", "B", "C", "D", "E", "F", "G")),
# #       numericInput("amount", "Loan Amount", value = 10000),
# #       numericInput("rate", "Interest Rate (%)", value = 10),
# #       numericInput("cred_len", "Credit History Length", value = 5),
# #       selectInput("default", "Previous Default",
# #                   choices = c("Y", "N")),
      
# #       actionButton("predict", "Predict Loan Status")
# #     ),
    
# #     mainPanel(
# #       h3("Prediction Result"),
# #       verbatimTextOutput("result"),
# #       verbatimTextOutput("prob")
# #     )
# #   )
# # )

# # server <- function(input, output) {
  
# #   observeEvent(input$predict, {
    
# #     user_data <- data.frame(
# #       person_age = input$age,
# #       person_income = input$income,
# #       person_home_ownership = input$home,
# #       person_emp_length = input$emp_len,
# #       loan_intent = input$intent,
# #       loan_grade = input$grade,
# #       loan_amnt = input$amount,
# #       loan_int_rate = input$rate,
# #       cb_person_cred_hist_length = input$cred_len,
# #       cb_person_default_on_file = input$default
# #     )
    
# #     prediction <- predict(model, user_data, type = "response")
# #     probability <- predict(model, user_data, type = "prob")[,2]
    
# #     output$result <- renderText({
# #       if (prediction == 1) {
# #         "❌ Loan Will Default"
# #       } else {
# #         "✅ Loan Will Be Repaid"
# #       }
# #     })
    
# #     output$prob <- renderText({
# #       paste("Probability of Default:", round(probability * 100, 2), "%")
# #     })
# #   })
# # }

# # shinyApp(ui = ui, server = server)


# library(shiny)
# library(randomForest)

# # Load trained model
# model <- readRDS("../models/loan_default_rf_model.rds")

# # ---- FIXED FACTOR LEVELS (MATCH TRAINING DATA) ----
# home_levels   <- c("RENT", "OWN", "MORTGAGE", "OTHER")
# intent_levels <- c("PERSONAL", "EDUCATION", "MEDICAL",
#                    "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")
# grade_levels  <- c("A", "B", "C", "D", "E", "F", "G")
# default_levels <- c("Y", "N")

# ui <- fluidPage(
#   titlePanel("Loan Default Prediction System"),

#   sidebarLayout(
#     sidebarPanel(
#       numericInput("age", "Age", 30, min = 18),
#       numericInput("income", "Annual Income", 50000),

#       selectInput("home", "Home Ownership", home_levels),
#       numericInput("emp_len", "Employment Length (years)", 5),

#       selectInput("intent", "Loan Intent", intent_levels),
#       selectInput("grade", "Loan Grade", grade_levels),

#       numericInput("amount", "Loan Amount", 10000),
#       numericInput("rate", "Interest Rate (%)", 10),
#       numericInput("cred_len", "Credit History Length", 5),

#       selectInput("default", "Previous Default", default_levels),

#       actionButton("predict", "Predict Loan Status")
#     ),

#     mainPanel(
#       h3("Prediction Result"),
#       textOutput("result"),
#       textOutput("probability")
#     )
#   )
# )

# server <- function(input, output) {

#   observeEvent(input$predict, {

#     # ---- CREATE INPUT EXACTLY LIKE TRAINING DATA ----
#     user_data <- data.frame(
#       person_age = as.numeric(input$age),
#       person_income = as.numeric(input$income),
#       person_home_ownership = factor(input$home, levels = home_levels),
#       person_emp_length = as.numeric(input$emp_len),
#       loan_intent = factor(input$intent, levels = intent_levels),
#       loan_grade = factor(input$grade, levels = grade_levels),
#       loan_amnt = as.numeric(input$amount),
#       loan_int_rate = as.numeric(input$rate),
#       loan_percent_income = input$amount / input$income,   # REQUIRED
#       cb_person_cred_hist_length = as.numeric(input$cred_len),
#       cb_person_default_on_file = factor(input$default, levels = default_levels)
#     )

#     # ---- PREDICT ----
#     pred <- predict(model, user_data)
#     prob <- predict(model, user_data, type = "prob")[, 2]

#     output$result <- renderText({
#    if (prob >= 0.7) {
#     "❌ Loan Will Default (High Risk)"
#    } else {
#     "✅ Loan Will Be Repaid"
#   }
# })

#     output$probability <- renderText({
#      paste("Probability of Default:", round(prob * 100, 2), "%")
#       })

#   })
# }
# shinyApp(ui = ui, server = server)




library(shiny)
library(randomForest)

# ---- LOAD TRAINED MODEL ----
model <- readRDS("../models/loan_default_rf_model.rds")

# ---- FIXED FACTOR LEVELS (MATCH TRAINING DATA) ----
home_levels    <- c("RENT", "OWN", "MORTGAGE", "OTHER")
intent_levels  <- c("PERSONAL", "EDUCATION", "MEDICAL",
                    "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")
grade_levels   <- c("A", "B", "C", "D", "E", "F", "G")
default_levels <- c("Y", "N")

# ===========================
#           UI
# ===========================
ui <- fluidPage(

  # ---- CUSTOM CSS ----
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #e3f2fd, #ffffff);
        font-family: 'Segoe UI', sans-serif;
      }

      .title-box {
        background: linear-gradient(90deg, #1565c0, #42a5f5);
        color: white;
        padding: 25px;
        border-radius: 15px;
        text-align: center;
        margin-bottom: 25px;
        box-shadow: 0px 5px 15px rgba(0,0,0,0.2);
      }

      .card {
        background: white;
        padding: 22px;
        border-radius: 15px;
        box-shadow: 0px 4px 18px rgba(0,0,0,0.12);
        margin-bottom: 20px;
      }

      .btn-custom {
        background: linear-gradient(90deg, #43a047, #66bb6a);
        color: white;
        font-size: 16px;
        font-weight: bold;
        width: 100%;
        border-radius: 12px;
        padding: 10px;
      }

      .result-good {
        color: #2e7d32;
        font-size: 24px;
        font-weight: bold;
      }

      .result-bad {
        color: #c62828;
        font-size: 24px;
        font-weight: bold;
      }

      .prob-text {
        font-size: 17px;
        color: #0d47a1;
        margin-top: 12px;
      }
    "))
  ),

  # ---- TITLE ----
  div(class = "title-box",
      h2("💳 Loan Default Prediction System"),
      h4("AI-Powered Risk Assessment using Random Forest")
  ),

  sidebarLayout(

    # ---- INPUT PANEL ----
    sidebarPanel(
      div(class = "card",

          numericInput("age", "Age", 30, min = 18),
          numericInput("income", "Annual Income", 50000),

          selectInput("home", "Home Ownership", home_levels),
          numericInput("emp_len", "Employment Length (years)", 5),

          selectInput("intent", "Loan Intent", intent_levels),
          selectInput("grade", "Loan Grade", grade_levels),

          numericInput("amount", "Loan Amount", 10000),
          numericInput("rate", "Interest Rate (%)", 10),
          numericInput("cred_len", "Credit History Length", 5),

          selectInput("default", "Previous Default", default_levels),

          actionButton("predict", "🔍 Predict Loan Status",
                       class = "btn-custom")
      ),
      width = 4
    ),

    # ---- OUTPUT PANEL ----
    mainPanel(
  div(class = "card",
      h3("📊 Prediction Result"),
      uiOutput("styled_result"),
      div(class = "prob-text",
          textOutput("probability")
      )
  ),
  width = 8
)

  )
)

# ===========================
#          SERVER
# ===========================
server <- function(input, output) {

  observeEvent(input$predict, {

    # ---- CREATE INPUT EXACTLY LIKE TRAINING DATA ----
    user_data <- data.frame(
      person_age = as.numeric(input$age),
      person_income = as.numeric(input$income),
      person_home_ownership = factor(input$home, levels = home_levels),
      person_emp_length = as.numeric(input$emp_len),
      loan_intent = factor(input$intent, levels = intent_levels),
      loan_grade = factor(input$grade, levels = grade_levels),
      loan_amnt = as.numeric(input$amount),
      loan_int_rate = as.numeric(input$rate),
      loan_percent_income = input$amount / input$income,
      cb_person_cred_hist_length = as.numeric(input$cred_len),
      cb_person_default_on_file = factor(input$default, levels = default_levels)
    )

    # ---- PREDICTION ----
    prob <- predict(model, user_data, type = "prob")[, 2]

    output$styled_result <- renderUI({
      if (prob >= 0.7) {
        div(class = "result-bad",
            "❌ Loan Will Default (High Risk)")
      } else {
        div(class = "result-good",
            "✅ Loan Will Be Repaid")
      }
    })

    output$probability <- renderText({
      paste("Probability of Default:", round(prob * 100, 2), "%")
    })

  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
