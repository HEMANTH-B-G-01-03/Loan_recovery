# library(shiny)
# library(randomForest)

# # Load trained model
# model <- readRDS("../models/loan_default_rf_model.rds")

# ui <- fluidPage(
  
#   titlePanel("Loan Default Prediction System"),
  
#   sidebarLayout(
    
#     sidebarPanel(
#       numericInput("age", "Age", value = 30, min = 18),
#       numericInput("income", "Annual Income", value = 50000),
#       selectInput("home", "Home Ownership",
#                   choices = c("RENT", "OWN", "MORTGAGE", "OTHER")),
#       numericInput("emp_len", "Employment Length (years)", value = 5),
#       selectInput("intent", "Loan Intent",
#                   choices = c("PERSONAL", "EDUCATION", "MEDICAL",
#                               "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")),
#       selectInput("grade", "Loan Grade",
#                   choices = c("A", "B", "C", "D", "E", "F", "G")),
#       numericInput("amount", "Loan Amount", value = 10000),
#       numericInput("rate", "Interest Rate (%)", value = 10),
#       numericInput("cred_len", "Credit History Length", value = 5),
#       selectInput("default", "Previous Default",
#                   choices = c("Y", "N")),
      
#       actionButton("predict", "Predict Loan Status")
#     ),
    
#     mainPanel(
#       h3("Prediction Result"),
#       verbatimTextOutput("result"),
#       verbatimTextOutput("prob")
#     )
#   )
# )

# server <- function(input, output) {
  
#   observeEvent(input$predict, {
    
#     user_data <- data.frame(
#       person_age = input$age,
#       person_income = input$income,
#       person_home_ownership = input$home,
#       person_emp_length = input$emp_len,
#       loan_intent = input$intent,
#       loan_grade = input$grade,
#       loan_amnt = input$amount,
#       loan_int_rate = input$rate,
#       cb_person_cred_hist_length = input$cred_len,
#       cb_person_default_on_file = input$default
#     )
    
#     prediction <- predict(model, user_data, type = "response")
#     probability <- predict(model, user_data, type = "prob")[,2]
    
#     output$result <- renderText({
#       if (prediction == 1) {
#         "❌ Loan Will Default"
#       } else {
#         "✅ Loan Will Be Repaid"
#       }
#     })
    
#     output$prob <- renderText({
#       paste("Probability of Default:", round(probability * 100, 2), "%")
#     })
#   })
# }

# shinyApp(ui = ui, server = server)


library(shiny)
library(randomForest)

# Load trained model
model <- readRDS("../models/loan_default_rf_model.rds")

# ---- FIXED FACTOR LEVELS (MATCH TRAINING DATA) ----
home_levels   <- c("RENT", "OWN", "MORTGAGE", "OTHER")
intent_levels <- c("PERSONAL", "EDUCATION", "MEDICAL",
                   "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")
grade_levels  <- c("A", "B", "C", "D", "E", "F", "G")
default_levels <- c("Y", "N")

ui <- fluidPage(
  titlePanel("Loan Default Prediction System"),

  sidebarLayout(
    sidebarPanel(
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

      actionButton("predict", "Predict Loan Status")
    ),

    mainPanel(
      h3("Prediction Result"),
      textOutput("result"),
      textOutput("probability")
    )
  )
)

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
      loan_percent_income = input$amount / input$income,   # REQUIRED
      cb_person_cred_hist_length = as.numeric(input$cred_len),
      cb_person_default_on_file = factor(input$default, levels = default_levels)
    )

    # ---- PREDICT ----
    pred <- predict(model, user_data)
    prob <- predict(model, user_data, type = "prob")[, 2]

    output$result <- renderText({
   if (prob >= 0.7) {
    "❌ Loan Will Default (High Risk)"
   } else {
    "✅ Loan Will Be Repaid"
  }
})

    output$probability <- renderText({
     paste("Probability of Default:", round(prob * 100, 2), "%")
      })

  })
}



shinyApp(ui = ui, server = server)
