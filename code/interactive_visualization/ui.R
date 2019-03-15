library(ggvis)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinyjs)
## template from here
## https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer

ui <-

  fluidPage(
    tags$head(
      tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
        type = "text/javascript"
      )
    ),
    tags$div(style="margin:50px auto; zoom:80%; scale: 0.9",
    useShinyjs(),
    titlePanel("Wealth Tax Explorer"),

    numericInput("extraBrackets", "Number of Brackets", 4, min = 2, max = 8),
    textInput("evasion", label = "Tax Evasion (%)", value = "16"),

    actionButton("reset", "Reset"),
    actionButton("submit", "Update"),
    htmlOutput("warn"),
    fluidRow(
      column(
        2,
        wellPanel(
          textInput("bracket1T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "0"),

          textInput("bracket2T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "2"),


          textInput("bracket3T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "2"),


          textInput("bracket4T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),




          uiOutput("myui")
        ),



        h6("Policy Analysis by:"),
        tags$a(href = "https://eml.berkeley.edu/~saez/", target = "_blank", "Emmanuel Saez"),
        h6("and"),
        tags$a(href = "http://gabriel-zucman.eu/", target = "_blank", "Gabriel Zucman"),
        h6("Interactive Visualization by:"),
        tags$a(href = "https://sastoudt.github.io", target = "_blank", "Sara Stoudt"),
        h6(""),
        h6("Assisted by:"),
        h6(""),
        h6("Katie Donnelly Moran"),
        h6("and Clancy Green,"),

        h6("Deployment help by:"),
        h6("Akcan Balkir"),
        tags$a(href = "https://mybinder.readthedocs.io/en/latest/", target = "_blank", "and the Binder Team"),

        h6(""),
        tags$a(href = "https://www.splitwise.com/taxes/#/brackets/0|160|353|432|479|543/10.1|14.9|25.0|28.1|33#.0|35.1/params/1|1|1|0|1|15", target = "_blank", "Inspired by this visualization")
      ),
      column(
        2,
        wellPanel(
          textInput("bracketV1T", label = HTML("to wealth above ($m): <br/> <br/>"), value = "10"),




          textInput("bracketV2T", label = HTML("to wealth above ($m): <br/> <br/>"), value = "50"),

          textInput("bracketV3T", label = HTML("to wealth above ($m): <br/> <br/>"), value = "500"),



          textInput("bracketV4T", label = HTML("to wealth above ($m): <br/> <br/>"), value = "1000"),



          uiOutput("myui2")
        ),
        h6("This visualization is part of an Open Policy Analysis supported by the"),
        tags$a(href = "https://www.bitss.org/opa/", target = "_blank", "Berkeley Initiative for Transparency in the Social Sciences."),
        h6(""),
        tags$a(href = "http://wealthtaxsimulator.org/analysis/", target = "_blank", "Detailed dynamic documentation here."),
        h6(""),
        tags$a(href = "https://github.com/BITSS/opa-wealthtax", target = "_blank", "Source code here."),
        h6("")
      ),

      column(
        8,
        ggvisOutput("plot2"),
        h3("Total Taxes ($bn)"),
        textOutput("totalTax"),
        h4("Total Taxes over 10 years ($t)"),
        textOutput("totalTax_10"),
        h4("Total Taxpayers"),
        textOutput("totalTaxpayers"),
        h4("Percentage of Families Affected"),
        textOutput("percentTaxUnits"),
        HTML("<div data-iframe-height></div>")
      )
    )
    )
  )