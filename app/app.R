library(shiny)
library(randomForest)

# Load trained model
model <- readRDS("../models/loan_default_rf_model.rds")

ui <- fluidPage(
  
  titlePanel("Loan Default Prediction System"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("age", "Age", value = 30, min = 18),
      numericInput("income", "Annual Income", value = 50000),
      selectInput("home", "Home Ownership",
                  choices = c("RENT", "OWN", "MORTGAGE", "OTHER")),
      numericInput("emp_len", "Employment Length (years)", value = 5),
      selectInput("intent", "Loan Intent",
                  choices = c("PERSONAL", "EDUCATION", "MEDICAL",
                              "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")),
      selectInput("grade", "Loan Grade",
                  choices = c("A", "B", "C", "D", "E", "F", "G")),
      numericInput("amount", "Loan Amount", value = 10000),
      numericInput("rate", "Interest Rate (%)", value = 10),
      numericInput("cred_len", "Credit History Length", value = 5),
      selectInput("default", "Previous Default",
                  choices = c("Y", "N")),
      
      actionButton("predict", "Predict Loan Status")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("result"),
      verbatimTextOutput("prob")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    user_data <- data.frame(
      person_age = input$age,
      person_income = input$income,
      person_home_ownership = input$home,
      person_emp_length = input$emp_len,
      loan_intent = input$intent,
      loan_grade = input$grade,
      loan_amnt = input$amount,
      loan_int_rate = input$rate,
      cb_person_cred_hist_length = input$cred_len,
      cb_person_default_on_file = input$default
    )
    
    prediction <- predict(model, user_data, type = "response")
    probability <- predict(model, user_data, type = "prob")[,2]
    
    output$result <- renderText({
      if (prediction == 1) {
        "❌ Loan Will Default"
      } else {
        "✅ Loan Will Be Repaid"
      }
    })
    
    output$prob <- renderText({
      paste("Probability of Default:", round(probability * 100, 2), "%")
    })
  })
}

shinyApp(ui = ui, server = server)
