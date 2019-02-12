library(ggvis)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
ui <-

  fluidPage(
    titlePanel("Tax Rates"),
    tags$a(href = "https://sastoudt.github.io", "Proof of concept visualization by Sara Stoudt"),
    h6(""),
    tags$a(href = "https://www.splitwise.com/taxes/#/brackets/0|160|353|432|479|543/10.1|14.9|25.0|28.1|33#.0|35.1/params/1|1|1|0|1|15", "Inspired by this visualization"),


    fluidRow(
      column(
        2,
        wellPanel(
          ## can set different defaults if desired
          
          sliderInput("bracketV1", label = "First Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value = c(10, 25)),
          sliderInput("bracketV2", label = "Second Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value = c(25, 50)),
          sliderInput("bracketV3", label = "Third Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value = c(50, 150)),
          sliderInput("bracketV4", label = "Last Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value=150),
          textOutput("warn")
          
        )
      ),
      
      column(2,
             wellPanel(
               
               sliderInput("bracket1", "Marginal Tax Rate in First Bracket",
                           0, 5, 0,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket2", "Marginal Tax Rate in Second Bracket",
                           0, 5, 0,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket3", "Marginal Tax Rate in Third Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket4", "Marginal Tax Rate in Fourth Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               )
             )
             
             ),#),#

      column(
        8,
        ggvisOutput("plot2"),
        # Format changes?
        # wellPanel(
        h3("Total Taxes ($bn)"),
        textOutput("totalTax"),
        h4("Total Taxes over 10 years ($t)"),
        textOutput("totalTax_10"),
        h4("Total Taxpayers (Households)"),
        textOutput("totalTaxpayers"),
        h4("Percentage of Households Affected"),
        textOutput("percentHouseAffected"),
        h4("Percentage Tax Units"),
        textOutput("percentTaxUnits")
        # )
      )
    )
  )