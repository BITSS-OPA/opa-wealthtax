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
                      max = 1000,step =5, value = c(10, 25)),
          sliderInput("bracketV3", label = "Third Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value = c(10, 25)),
          sliderInput("bracketV4", label = "First Tax Bracket (million)", min = 0, 
                      max = 1000,step =5, value = c(10, 25))
          
        )
      ),
      
      column(2,
             wellPanel(
               
               sliderInput("bracket1", "Marginal Tax Rate in $10m-$25m Bracket",
                           0, 5, 0,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket2", "Marginal Tax Rate in $25m-$50m Bracket",
                           0, 5, 0,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket3", "Marginal Tax Rate in $50m-$100m Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket4", "Marginal Tax Rate in $100m-$250m Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket5", "Marginal Tax Rate in $250m-$500m Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket6", "Marginal Tax Rate in $500m-$1bn Bracket",
                           0, 5, 2,
                           step = 0.1#, ticks = F
               ),
               sliderInput("bracket7", "Marginal Tax Rate in $1bn+ Bracket",
                           0, 5, 3,
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