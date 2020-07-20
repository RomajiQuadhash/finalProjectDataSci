# program by Romaji Milton Amulo, adapted from the k-means example in the Shiny
# gallery. 
# Data from https://archive.ics.uci.edu/ml/datasets/Combined+Cycle+Power+Plant

# The purpose of this tool is to show how to visualize how different "true" clusters,
# that is, the ones formed 

# From the original data, called myData 
# (from the original data tables, particularly page one), here's how I came up with the data being loaded here
# transData = prcomp(myData)
# combined = merge(myData,transData$x) # Never actually worked, so I had to "cheat"
#                                      # This cheat came from exporting the transData$x as a csv file,
#                                      # Then merging the two files in exel.
# write.csv(combined,"powerplantPlus.csv")
# write.csv(transData$center, "centerPoint.csv")
# write.csv(transData$rotation, "rotation.csv")
powerplant <- read.csv("powerplantPlus.csv")
vars <- names(powerplant)
normal <- c("AT","V","AP","RH","PE") #Variables in the original set
rotation <- data.matrix(data.frame(read.csv("rotation.csv"),row.names = 1))
# convert from the format of a generic table to the kind that comes out if I ran prcomp
center <- t(data.matrix(data.frame(read.csv("centerPoint.csv"),row.names = 1)))
# again, getting into the right format.
library(ggplot2)
library(dplyr)


# Define the UI
ui <- fluidPage(
  titlePanel('Powerplant k-means clustering'),
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', vars),
      selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
      sliderInput("alpha", "Alpha of Points",min=0.001, max = 1, value=0.1),
      sliderInput("size", "Size of Points",min=0.1, max = 4, value=2),
      actionButton("stUpdate","Update Style")
    ),
    mainPanel(
      plotOutput('plot1', height = "550px")
    )
  ),
  mainPanel(width= 12,
            verbatimTextOutput('textInfo')
  )
)


# Define the server code
server <- function(input, output) {
  #computes the clusters for the whole data set
  clusters <- reactive({
    klusters = kmeans(powerplant[,normal], input$clusters) # make it from the data without the added variables
    newCenters= klusters$centers - center[rep(1,input$clusters),] #shift the centers.
    newCenters = newCenters %*% rotation #adjust the centers to the 
    klusters$centers = cbind(klusters$centers,newCenters)
    klusters
  })
  
  style <- eventReactive(input$stUpdate, { #Don't update the graph until they want you to.
    data.frame(size=input$size,alpha=input$alpha)
  },ignoreNULL = F) #run on start
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(powerplant,aes(eval(parse(text=input$xcol)),eval(parse(text=input$ycol)))) +
      geom_point(col = clusters()$cluster + 1,
                 size=style()$size, alpha=style()$alpha) +
      guides(x=guide_axis(input$xcol),y=guide_axis(input$ycol))+
      geom_point(data=as.data.frame(clusters()$centers),
                 aes(eval(parse(text=input$xcol)),eval(parse(text=input$ycol))),
                 pch = 4, cex = 4, lwd = 4)
  })
  
  output$textInfo <- renderPrint({
    if (!input$xcol %in% normal){
      print(paste("The x axis,",input$xcol,"comes from"))
      print(rotation[,input$xcol])
    } else {
      print(paste("The x axis,",input$xcol,"contributes to"))
      print(rotation[input$xcol,])
    }
    if (!input$ycol %in% normal){
      print(paste("The y axis,",input$ycol,"comes from"))
      print(rotation[,input$ycol])
    } else {
      print(paste("The y axis,",input$ycol,"contributes to"))
      print(rotation[input$ycol,])
    }
    print("The center is at")
    print(center)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
