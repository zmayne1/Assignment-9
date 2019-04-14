################################################################################################################################################################################################################################################################

# Load packages
if(!require(shiny)) {install.packages("shiny"); require(shiny)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(xgboost)) {install.packages("xgboost"); require(xgboost)}
if(!require(ModelMetrics)) {install.packages("ModelMetrics"); require(ModelMetrics)}
if(!require(shinythemes)) {install.packages("shinythemes"); require(shinythemes)}

################################################################################################################################################################################################################################################################

# Prepare data
x_train <- iris %>% select(-Species)
y_train <- as.numeric(factor(iris$Species)) - 1
x_test <- iris %>% select(-Species)
y_test <- as.numeric(factor(iris$Species)) - 1

################################################################################################################################################################################################################################################################

# Save variable names
var.names = names(x_train)
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

################################################################################################################################################################################################################################################################

# Prepare xgboost parameters
param <- list("objective" = "multi:softprob", 
              "eval_metric" = "mlogloss", 
              "num_class" = length(table(y_train)), 
              "eta" = .2,
              "max_depth" = 3, 
              "lambda" = 1, 
              "alpha" = 2,
              "min_child_weight" = 11, 
              "subsample" = .9, 
              "colsample_bytree" = .7)

################################################################################################################################################################################################################################################################

# Set up cross validation
bst.cv <- xgb.cv(param = param, data = x_train, label = y_train, nfold = 5, nrounds = 20000, missing = NA, prediction = TRUE)

################################################################################################################################################################################################################################################################

# Select which round(s) had the lowest/best log loss value
nround <- which(bst.cv$evaluation_log$test_mlogloss_mean == min(bst.cv$evaluation_log$test_mlogloss_mean))

################################################################################################################################################################################################################################################################

# Build classifier
irisClassifier <- xgboost(params = param, data = x_train, label = y_train, nrounds = nround, missing = NA)

################################################################################################################################################################################################################################################################

# Define UI for slider demo app ----
ui <- fluidPage(theme = shinytheme("darkly"),
  
  # App title ----
  titlePanel("Iris Species Identification"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      sliderInput("petal.length",
                  "Petal Length:",
                  min = 1,
                  max = 6.9,
                  value = 3.76,
                  step = .1),
      sliderInput("petal.width",
                  "Petal Width",
                  min = .1,
                  max = 2.5,
                  value = 1.2,
                  step = .1),
      sliderInput("sepal.length",
                  "Sepal Length",
                  min = 4.3,
                  max = 7.9,
                  value = 5.84,
                  step = .1),
      sliderInput("sepal.width",
                  "Sepal Width",
                  min = 2,
                  max = 4.4,
                  value = 3.06,
                  step = .1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("tbl1"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),
      plotOutput("plot5")
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  probTable <- reactive({
    irises <- data.frame(
      Species = c("Setosa",
                  "Versicolor",
                  "Virginica"),
      Probability = c(predict(irisClassifier, matrix(input$petal.length, input$petal.width, input$sepal.length, input$sepal.width))[1],
                      predict(irisClassifier, matrix(input$petal.length, input$petal.width, input$sepal.length, input$sepal.width))[2],
                      predict(irisClassifier, matrix(input$petal.length, input$petal.width, input$sepal.length, input$sepal.width))[3])
      )
    
    irises %>% arrange(-Probability)
  })
  
  output$tbl1 <- renderTable({
    probTable()
  })
  
  output$plot1 <- renderPlot({
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
      geom_point() +
      geom_point(aes(x = 5.8, y = 3.1), color = "red", size = 3) +
      theme_dark() +
      xlab(label = "Sepal Length") +
      ylab(label = "Sepal Width")  +
      theme(plot.background = element_rect(fill = "grey 24"))
    })
  
  output$plot2 <- renderPlot({
    ggplot(iris, aes(Petal.Length, color = Species)) +
      geom_density() +
      theme_dark() +
      xlab(label = "Petal Length") +
      ylab(label = "Density") +
      geom_vline(aes(xintercept = 3.76), color = "red") +
      theme(plot.background = element_rect(fill = "grey 24"))
  })
  
  output$plot3 <- renderPlot({
    ggplot(iris, aes(Petal.Width, color = Species)) +
      geom_density() +
      theme_dark() +
      xlab(label = "Petal Width") +
      ylab(label = "Density") +
      geom_vline(aes(xintercept = 1.2), color = "red") +
      theme(plot.background = element_rect(fill = "grey 24"))
  })
  
  output$plot4 <- renderPlot({
    ggplot(iris, aes(Sepal.Length, color = Species)) +
      geom_density() +
      theme_dark() +
      xlab(label = "Sepal Length") +
      ylab(label = "Density") +
      geom_vline(aes(xintercept = 5.84), color = "red") +
      theme(plot.background = element_rect(fill = "grey 24"))
  })
  
  output$plot5 <- renderPlot({
    ggplot(iris, aes(Sepal.Width, color = Species)) +
      geom_density() +
      theme_dark() +
      xlab(label = "Sepal Width") +
      ylab(label = "Density") +
      geom_vline(aes(xintercept = 3.06), color = "red") +
      theme(plot.background = element_rect(fill = "grey 24"))
  })
}

# Create Shiny app ----
shinyApp(ui, server)
