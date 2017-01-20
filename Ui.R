library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("blockchain"),
  
  sidebarLayout(position = "right",
    sidebarPanel("sidebar panel"),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.",style="font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue")
      )
  )
))