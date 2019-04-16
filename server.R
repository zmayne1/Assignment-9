# Define server logic for slider examples ----
shinyServer(function(input, output) {
  
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
)
