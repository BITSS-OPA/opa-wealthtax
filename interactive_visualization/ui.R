library(ggvis)
library(tidyr)
library(dplyr)
library(ggplot2)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer

##https://stackoverflow.com/questions/39627760/conditional-panel-in-shiny-doesnt-update-variables
ui <-

  fluidPage(
    titlePanel("Wealth Tax Explorer"),
    #radioButtons("interface", "What interface do you prefer?", c("Sliders" = 1, "Manual Input" = 2), selected = 1),
    numericInput("extraBrackets", "Extra Brackets?", 0, min = 0, max = 4),
    textInput("evasion", label = "Tax Evasion (%)", value = "16"),
 
   # submitButton("Apply Changes"),
    
    fluidRow(
      column(
        2,
        wellPanel(
            textInput("bracket1T", label = "Apply a tax of (%):", value = "0"),

            textInput("bracket2T", label = "Apply a tax of (%):", value = "2"),

          
            textInput("bracket3T", label = "Apply a tax of (%):", value = "2"),

          
            textInput("bracket4T", label = "Apply a tax of (%):", value = "3"),


          

          uiOutput("myui")),



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
          
            textInput("bracketV1T", label = "to wealth above ($m):", value = "10"),
          


          
            textInput("bracketV2T", label = "to wealth above ($m):", value = "50"),
         
            textInput("bracketV3T", label = "to wealth above ($m):", value = "500"),


          
            textInput("bracketV4T", label = "to wealth above ($m):", value = "1000"),
         

         
            uiOutput("myui2")
          
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
        ggvisOutput("plot2"), ## comment out for now until ui is working
        # ggvisOutput("plotB"),

        h3("Total Taxes ($bn)"),
        textOutput("totalTax"),
        h4("Total Taxes over 10 years ($t)"),
        textOutput("totalTax_10"),
        h4("Total Taxpayers"), ##  (CHECK UNITS)
        textOutput("totalTaxpayers"),
        h4("Percentage of Tax Units Affected"), ## (CHECK UNITS)
        textOutput("percentTaxUnits")
      )
    )
  )