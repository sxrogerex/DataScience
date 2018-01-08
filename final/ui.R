library(shiny)
shinyUI(
  navbarPage(
    title = "105356002 final project",
  tabPanel('Data', 
           titlePanel("279 rows from online questionnaire"),
           tableOutput("data")),
  tabPanel('Visualize Data',  
           plotOutput("willPay"),
           plotOutput("freqSex"),
           plotOutput("incomeSex")),
  tabPanel('Spearman rank correlation',
           plotOutput("spearman"),
           titlePanel("p of frequency all > 0.05, can't reject h0"),
           titlePanel("all p between income and other clothes < 0.05, reject h0, but correlation coefficient all between 0.16~0.25, Modestly correlated"),
           titlePanel("all p between sex and other clothes < 0.05, reject h0, but correlation coefficient all between 0.20~0.34, Modestly correlated")
          ),
  tabPanel('Linear Regression with 5-fold validation',
           # numericInput("num", label = h3("?-fold validation"), value = 1,min = 1 ),
           # radioButtons("y", label = h3("Choose the Y"),
           #              choices = list("Short Sleeve" = 1, "Long Sleeved Shirt" = 2, "Shorts" = 3,"Trousers"=4,"Hoodies"=5,"Coat"=6), 
           #               selected = 1),
           # actionButton("action", label = "Action"),
           titlePanel("Train result"),
           tableOutput("trainResult"),
           titlePanel("Valid result"),
           tableOutput("validResult"),
           titlePanel("Test result"),
           tableOutput("testResult"),
           titlePanel("p value"),
           plotOutput("p"),
           titlePanel("R square"),
           plotOutput("r")
           )
  )
)