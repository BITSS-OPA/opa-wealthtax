library(ggvis)
library(tidyr)
library(dplyr)
library(ggplot2)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer
ui <-

  fluidPage(
    titlePanel("Wealth Tax Explorer"),
    radioButtons("interface", "What interface do you prefer?", c("Sliders"=1,"Manual Input"=2),selected = 1),
    fluidRow(
      column(
        2,
        wellPanel(
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracket1", "Apply a tax of:",
                        0, 10, 0,
                        step = .1, post = " %" # , ticks = F
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracket1", label = "Apply a tax of (%):", value = "0")),
          
          
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracket2", "Apply a tax of:",
                        0, 10, 2,
                        step = .1, post = " %" # , ticks = F
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracket2", label = "Apply a tax of (%):", value = "2")),
          
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracket3", "Apply a tax of:",
                        0, 10, 2,
                        step = .1, post = " %" # , ticks = F
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracket3", label = "Apply a tax of (%):", value = "2")),
       
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracket4", "Apply a tax of:",
                        0, 10, 3,
                        step = .1, post = " %" # , ticks = F
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracket4", label = "Apply a tax of (%):", value = "3")),
          
          
          checkboxInput("extraBracket1", "Add a bracket?", value = F),
          
          conditionalPanel(
            condition = "input.extraBracket1 == true && input.interface ==1",
            sliderInput("bracket5", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),conditionalPanel(
            condition = "input.extraBracket1 == true && input.interface == 2",
            textInput("bracket5", label = "Apply a tax of (%):", value = "3")
          ),
          conditionalPanel(
            condition = "input.extraBracket1 == true",
            checkboxInput("extraBracket2", "Add a bracket?", value = FALSE)
          ),
          
          
          conditionalPanel(
            condition = "input.extraBracket2 == true && input.interface ==1",
            sliderInput("bracket6", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),conditionalPanel(
            condition = "input.extraBracket2 == true && input.interface==2",
            textInput("bracket6", label = "Apply a tax of (%):", value = "3")
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true",
            checkboxInput("extraBracket3", "Add a bracket?", value = FALSE)
          ),
          
          
          conditionalPanel(
            condition = "input.extraBracket3 == true && input.interface==1",
            sliderInput("bracket7", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),conditionalPanel(
            condition = "input.extraBracket3 == true && input.interface==2",
            textInput("bracket7", label = "Apply a tax of (%):", value = "3")
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true",
            checkboxInput("extraBracket4", "Add a bracket?", value = FALSE)
          ),
          
          
          conditionalPanel(
            condition = "input.extraBracket4 == true && input.interface==1",
            sliderInput("bracket8", "Apply a tax of:",
              0, 10, 3,
              step = .1, post = " %" # , ticks = F
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true && input.interface==2",
            textInput("bracket8", label = "Apply a tax of (%):", value = "3")
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
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracketV1",
                        label = "to wealth above: ", min = 0,
                        max = 1000, step = 5, value = 10, post = " (m)"
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracketV1", label = "to wealth above (in millions):", value = "10")),

          
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracketV2",
                        label = "to wealth above: ", min = 0,
                        max = 1000, step = 5, value = 50, post = " (m)"
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracketV2", label = "to wealth above (in millions)", value = "50")),
          
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracketV3",
                        label = "to wealth above:", min = 500,
                        max = 1500, step = 5, value = 500, post = " (m)"
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracketV3", label = "to wealth above (in millions)", value = "500")),
          
          
          conditionalPanel(
            condition = "input.interface == 1",
            sliderInput("bracketV4",
                        label = "to wealth above:", min = 1000,
                        max = 10000, step = 100, value = 1000, post = " (m)"
            )),
          conditionalPanel(
            condition = "input.interface == 2",
            textInput("bracketV4", label = "to wealth above (in millions)", value = "1000")),
          
          conditionalPanel(
            condition = "input.extraBracket1 == true && input.interface==1",
            sliderInput("bracketV5",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1500, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket1 == true && input.interface==2",
            textInput("bracketV5", label = "to wealth above (in millions)", value = "1500")
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true && input.interface==1",
            sliderInput("bracketV6",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1750, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket2 == true &&  input.interface==2",
            textInput("bracketV6", label = "to wealth above (in millions)", value = "1750")
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true && input.interface==1",
            sliderInput("bracketV7 ",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 1900, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket3 == true && input.interface==2",
            textInput("bracketV7", label = "to wealth above (in millions)", value = "1900")
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true && input.interface==1",
            sliderInput("bracketV8",
              label = "to wealth above:", min = 1000,
              max = 10000, step = 100, value = 2000, post = " (m)"
            )
          ),
          conditionalPanel(
            condition = "input.extraBracket4 == true && input.interface==2",
            textInput("bracketV8", label = "to wealth above (in millions)", value = "2000")
          )
        ),
        h6("Assisted by:"),
        h6(""),
        h6("Katie Donnelly Moran"),
        h6("and Clancy Green,"),

        h6("Deployment help by:"),
        h6("Akcan Balkir,"),
        h6("Lindsey Heagy,"),
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