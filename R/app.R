library(shiny)
library(ggplot2)

source('roi_function.R')
source('create_simulated_data.R')

ui <- fluidPage(
  verticalLayout(
    titlePanel("Simulated Harmonization"),
    flowLayout(
      tableOutput("table")
    ),
    sidebarLayout(
      # sidebar panel for controlling inputs
      sidebarPanel(
        radioButtons(inputId="stage",
                     label="Show ROIs Before/After Harmonization:",
                     choices=c("Before","After"),
                     inline=TRUE),
        sliderInput(inputId = "tau",
                    label = "Overall Noise",
                    value = 50,
                    min = 1,
                    max = 100),
        tabsetPanel(
          tabPanel("Site 1",
                   sliderInput(inputId = "site1_n",
                               label = "Sample Size",
                               value = 500,
                               min = 100,
                               max = 1000),
                   sliderInput(inputId = "site1_ages",
                               label = "Age Range",
                               value = c(10, 70),
                               min = 1,
                               max = 100),
                   sliderInput(inputId = "site1_sigma",
                               label = "Noise",
                               value = 5,
                               min = 1,
                               max = 100)
          ),
          tabPanel("Site 2",
                   sliderInput(inputId = "site2_n",
                               label = "Sample Size",
                               value = 200,
                               min = 100,
                               max = 1000),
                   sliderInput(inputId = "site2_ages",
                               label = "Age Range",
                               value = c(5, 25),
                               min = 1,
                               max = 100),
                   sliderInput(inputId = "site2_sigma",
                               label = "Noise",
                               value = 10,
                               min = 1,
                               max = 100),
                   sliderInput(inputId = "site2_mu",
                               label = "True Effect Size",
                               value = 500,
                               min = -1000,
                               max = 1000)
          ),
          tabPanel("Site 3",
                   sliderInput(inputId = "site3_n",
                               label = "Sample Size",
                               value = 100,
                               min = 100,
                               max = 1000),
                   sliderInput(inputId = "site3_ages",
                               label = "Age Range",
                               value = c(50, 100),
                               min = 1,
                               max = 100),
                   sliderInput(inputId = "site3_sigma",
                               label = "Noise",
                               value = 10,
                               min = 1,
                               max = 100),
                   sliderInput(inputId = "site3_mu",
                               label = "True Effect Size",
                               value = -350,
                               min = -1000,
                               max = 1000)
          ))
      ),
      
      # main panel for displaying output
      mainPanel(
        plotOutput("density"),
        plotOutput("scatter")
      )    
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    create_simulated_data(N=c(input$site1_n,
                              input$site2_n,
                              input$site3_n),
                          ages=list(input$site1_ages,
                                    input$site2_ages,
                                    input$site3_ages),
                          sigma=c(input$site1_sigma,
                                  input$site2_sigma,
                                  input$site3_sigma),
                          tau=input$tau,
                          mu=c(0,input$site2_mu,input$site3_mu))
  })
  # build objects with render*() functions
  output$density <- renderPlot({
    p<-ggplot(data(), aes(x=Age, color=Site, fill=Site)) +
      geom_density(alpha=0.5)
    p + theme_light()
  })
  output$scatter <- renderPlot({
    if (input$stage=="Before"){
      p<-ggplot(data(), aes(x=Age, y=y.roi, color=Site)) +
        geom_point()
      p + theme_light()
    } else {
      p<-ggplot(data(), aes(x=Age, y=y.roi.corrected, color=Site)) +
        geom_point()
      p + theme_light()
    }
  })
  output$table <- renderTable({
    df <- data()
    t <- data.frame(aggregate(Age ~ Site, data=df, FUN=length))
    colnames(t) <- c('Site','Sample.size')
    t$Age.mean <- aggregate(Age ~ Site, data=df, FUN=mean)[,2]
    t$ROI.mean <- aggregate(y.roi ~ Site, data=df, FUN=mean)[,2]
    t$True.effect <- c(0,input$site2_mu,input$site3_mu)
    t$Est.effect <- aggregate(Site.effect ~ Site, data=df, FUN=mean)[,2]
    t$Abs.error <- abs(t$True.effect - t$Est.effect)
    t$APE <- t$Abs.error / abs(t$True.effect)
    # t$RMSE <- performance(df)
    # t$`RMSE.prop` <- (t$RMSE / t$ROI.mean)
    t$`MedAPE*` <- c(NA, NA, median(t$APE[2:3]))
    t
  })
}

shinyApp(ui = ui, server = server)
