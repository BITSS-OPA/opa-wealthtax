library(ggvis)
library(tidyr)
library(dplyr)
library(ggplot2)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
ui <-

  fluidPage(
    titlePanel("Tax Rates"),
    tags$a(href = "https://sastoudt.github.io", "Visualization by Sara Stoudt"),
    h6(""),
    tags$a(href = "https://www.splitwise.com/taxes/#/brackets/0|160|353|432|479|543/10.1|14.9|25.0|28.1|33#.0|35.1/params/1|1|1|0|1|15", "Inspired by this visualization"),


    fluidRow(
      column(
        2,
        wellPanel(
          sliderInput("bracketV1",
            label = "First Tax Bracket (million)", min = 0,
            max = 1000, step = 5, value = c(10, 50)
          ),
          textOutput("warn1"),
          sliderInput("bracketV2",
            label = "Second Tax Bracket (million)", min = 0,
            max = 1000, step = 5, value = c(50, 500)
          ),
          textOutput("warn2"),
          sliderInput("bracketV3",
            label = "Third Tax Bracket (million)", min = 500,
            max = 1500, step = 5, value = c(500, 1000)
          ),
          textOutput("warn3"),
          sliderInput("bracketV4",
            label = "Last Tax Bracket (million)", min = 1000,
            max = 10000, step = 100, value = 1000
          )
        )
      ),

      column(
        2,
        wellPanel(
          sliderInput("bracket1", "Marginal Tax Rate in Bracket 1",
            0, 10, 1,
            step = 1 # , ticks = F
          ),
          sliderInput("bracket2", "Marginal Tax Rate in Bracket 2",
            0, 10, 1,
            step = 1 # , ticks = F
          ),
          sliderInput("bracket3", "Marginal Tax Rate in Bracket 3",
            0, 10, 1,
            step = 1 # , ticks = F
          ),
          sliderInput("bracket4", "Marginal Tax Rate in Bracket 4",
            0, 10, 2,
            step = 1 # , ticks = F
          )
        )
      ),

      column(
        8,
        ggvisOutput("plot2"),
        ggvisOutput("plotB"),

        h3("Total Taxes ($bn)"),
        textOutput("totalTax"),
        h4("Total Taxes over 10 years ($t)"),
        textOutput("totalTax_10"),
        h4("Total Taxpayers (CHECK UNITS)"),
        textOutput("totalTaxpayers"),
        h4("Percentage of (CHECK UNITS) Affected"),
        textOutput("percentHouseAffected")
      )
    )
  )