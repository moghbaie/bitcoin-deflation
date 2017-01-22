library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Deflationary effect of Bitcoin on US economy "),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(

  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    plotOutput("Plot1"),
    plotOutput("Plot2"),
    plotOutput("Plot3"),
    plotOutput("Plot7"),
    #plotOutput("Plot8"),
    plotOutput("Plot9"),
    plotOutput("Plot10"),
    plotOutput("Plot11"),
    plotOutput("Plot12"),
    plotOutput("Plot13"),
    plotOutput("Plot14"),
    plotOutput("Plot15"),
    plotOutput("Plot16"),
    plotOutput("Plot17"),
    plotOutput("Plot18"),
    plotOutput("Plot19"),
    plotOutput("Plot20"),
    plotOutput("Plot21"),
    plotOutput("Plot22"),
    plotOutput("Plot23"),
    plotOutput("Plot24"),
    plotOutput("Plot25"),
    plotOutput("Plot26"),
    plotOutput("Plot27"),
    plotOutput("Plot28"),
    plotOutput("Plot29"),
    plotOutput("Plot30"),
    plotOutput("Plot33"),
    plotOutput("Plot34"),
    plotOutput("Plot35"),
    plotOutput("Plot36")
  )
))