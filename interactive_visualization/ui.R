library(ggvis)
library(tidyr)
library(dplyr)
library(ggplot2)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
ui <-

  fluidPage(
    titlePanel("Wealth Tax Explorer"),

    fluidRow(
      column(
        2,
        wellPanel(
          sliderInput("bracket1", "Apply a tax of:",
            0, 10, 0,
            step = .1, post = " %" # , ticks = F
          ),
          textInput("bracket1T", label = "", value = "0"),

          sliderInput("bracket2", "Apply a tax of:",
            0, 10, 2,
            step = .1, post = " %" # , ticks = F
          ),
          textInput("bracket2T", label = "", value = "2"),

          sliderInput("bracket3", "Apply a tax of:",
            0, 10, 2,
            step = .1, post = " %" # , ticks = F
          ),
          textInput("bracket3T", label = "", value = "2"),

          sliderInput("bracket4", "Apply a tax of:",
            0, 10, 3,
            step = .1, post = " %" # , ticks = F
          ),
          textInput("bracket4T", label = "", value = "3"),
          checkboxInput("extraBracket1", "Add a bracket?", value = F),
          conditionalPanel(
            condition = "input.extraBracket1 == true",
            sliderInput("bracket5", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket1 == true",
            checkboxInput("extraBracket2", "Add a bracket?", value = FALSE)
          ), ## repeat 3 more times after we get server working
          conditionalPanel(
            condition = "input.extraBracket2 == true",
            sliderInput("bracket6", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true",
            checkboxInput("extraBracket3", "Add a bracket?", value = FALSE)
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true",
            sliderInput("bracket7", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true",
            checkboxInput("extraBracket4", "Add a bracket?", value = FALSE)
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true",
            sliderInput("bracket8", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          )
        ),



        h6("Policy Analysis by:"),
        tags$a(href = "https://eml.berkeley.edu/~saez/", "Emmanuel Saez"),
        h6("and"),
        tags$a(href = "http://gabriel-zucman.eu/", "Gabriel Zucman"),
        h6("Interactive Visualization by:"),
        tags$a(href = "https://sastoudt.github.io", "Sara Stoudt"),
        h6(""),
        tags$a(href = "https://github.com/fhoces/opa-wealthtax", "Open Policy Analysis by"),
        h6(""),
        tags$a(href = "https://fhoces.github.io/", "Fernando Hoces de la Guardia")
      ),
      column(
        2,
        wellPanel(
          sliderInput("bracketV1",
            label = "to wealth above: ", min = 0,
            max = 1000, step = 5, value = 10, post = " (m)"
          ),
          textInput("bracketV1T", label = "", value = "10"),

          sliderInput("bracketV2",
            label = "to wealth above: ", min = 0,
            max = 1000, step = 5, value = 50, post = " (m)"
          ),
          textInput("bracketV2T", label = "", value = "50"),

          sliderInput("bracketV3",
            label = "to wealth above:", min = 500,
            max = 1500, step = 5, value = 500, post = " (m)"
          ),
          textInput("bracketV3T", label = "", value = "500"),

          sliderInput("bracketV4",
            label = "to wealth above:", min = 1000,
            max = 10000, step = 100, value = 1000, post = " (m)"
          ),
          textInput("bracketV4T", label = "", value = "1000"),
          conditionalPanel(
            condition = "input.extraBracket1 == true",
            sliderInput("bracketV5",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1500, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket1 == true",
            textInput("bracketV5T", label = "", value = "1500")
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true",
            sliderInput("bracketV6",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1750, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true",
            textInput("bracketV6T", label = "", value = "1750")
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true",
            sliderInput("bracketV7",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1900, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true",
            textInput("bracketV8T", label = "", value = "1900")
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true",
            sliderInput("bracketV8",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 2000, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true",
            textInput("bracketV8T", label = "", value = "2000")
          )
        ),
        h6("Assisted by:"),
        h6(""),
        h6("Katie Donnelly Moran"),
        h6("and Clancy Green,"),

        h6("Deployment help by:"),
        h6("Akcan Balkir,"),
        h6("Lindsey Heagy"),
        h6("Chris Holdgraf, and"),
        h6("Yuvi Panda"),

        tags$a(href = "https://www.splitwise.com/taxes/#/brackets/0|160|353|432|479|543/10.1|14.9|25.0|28.1|33#.0|35.1/params/1|1|1|0|1|15", "Inspired by this visualization")
      ),

      column(
        8,
        ggvisOutput("plot2"),
        # ggvisOutput("plotB"),

        h3("Total Taxes ($bn)"),
        textOutput("totalTax"),
        h4("Total Taxes over 10 years ($t)"),
        textOutput("totalTax_10"),
        h4("Total Taxpayers"), ##  (CHECK UNITS)
        textOutput("totalTaxpayers"),
        h4("Percentage of Tas Units Affected"), ## (CHECK UNITS)
        textOutput("percentTaxUnits")
      )
    )
  )