library(ggvis)

ui <-

fluidPage(
  titlePanel("Tax Rates"),
  fluidRow(
    column(3,
           wellPanel(
             #h4("Filter"),
             sliderInput("bracket1", "Marginal Tax Rate in $10m-$25m Bracket",
                         0, 10, 0, step = 0.5),
             sliderInput("bracket2", "Marginal Tax Rate in $25m-$50m Bracket",
                         0, 10, 0, step = 0.5),
             sliderInput("bracket3", "Marginal Tax Rate in $50m-$100m Bracket",
                         0, 10, 2, step = 0.5),
             sliderInput("bracket4", "Marginal Tax Rate in $100m-$250m Bracket",
                         0, 10, 2, step = 0.5),
             sliderInput("bracket5", "Marginal Tax Rate in $250m-$500m Bracket",
                         0, 10, 2, step = 0.5),
             sliderInput("bracket6", "Marginal Tax Rate in $500m-$1bn Bracket",
                         0, 10, 2, step = 0.5),
             sliderInput("bracket7", "Marginal Tax Rate in $1bn+ Bracket",
                         0, 10, 3, step = 0.5)
             )
           )
       
    ,
    
    column(9,
           ggvisOutput("plot2"),
           #wellPanel(
             h4("Total Taxes ($bn)"),
             textOutput("totalTax"),
             h4("Total Taxpayers"),
             textOutput("totalTaxpayers"),
             h4("Percentage of Households Affected"),
             textOutput("percentHouseAffected"),
             h4("Percentage Tax Units"),
             textOutput("percentTaxUnits")
          # )
           )
    
    )
)