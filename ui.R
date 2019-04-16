# Define UI for dataset viewer application
shinyUI(
  fluidPage(theme = shinytheme("darkly"),
            
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
  
  )



