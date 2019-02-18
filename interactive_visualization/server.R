
server <- function(input, output, session) {
  #grid <- read.csv("taxBaseGrid.csv")
  grid <- read.csv("taxBaseGridUpdated.csv")
  # print(head(grid)) ## check that app has access to this file

  getTaxBasePerBracket <- function(grid, brackets) {
    ## brackets is lower end of each bracket
    brackets <- c(brackets, 1e10) ## get last bracket
    grid$total <- grid$nb*grid$avg
    grid$group <- cut(grid$thres, brackets)
    toReturn <- grid %>% group_by(group) %>% summarise(taxBase = sum(total)) %>% drop_na()

    
    return(toReturn)
  }

  getPeoplePerBracket <- function(grid, brackets) {
    ## brackets is lower end of each bracket
    brackets <- c(brackets, 1e10) ## get last bracket
    grid$group <- cut(grid$thres, brackets, include.lowest = T)
    toReturn <- grid %>% group_by(group) %>% summarise(totalPeople = sum(nb)) %>% drop_na()

    
    return(toReturn)
  }

  ### update tax brackets based on previous decisions
  observe({
    val <- bracketVal1()[2]
    updateSliderInput(session, "bracketV2",
      min = 0, value = c(val, max(val + 50, bracketVal2()[2])),
      max = 1000, step = 5
    )
    output$warn1 <- renderText({
      ""
    })
  })

  observe({
    val <- bracketVal1()[2]
    val2 <- bracketVal2()[1]

    if (val2 < val) {
      output$warn1 <- renderText({
        "Warning: First and second brackets overlap."
      })
    }
    if (val2 == val) {
      output$warn1 <- renderText({
        ""
      })
    }

    if (val2 > val) {
      output$warn1 <- renderText({
        "Warning: Gap between first and second brackets."
      })
    }
  })


  observe({
    # val <- input$bracketV2[2]
    val <- bracketVal2()[2]
    updateSliderInput(session, "bracketV3", value = c(val, max(val + 50, bracketVal3()[2])))
  })
  output$warn2 <- renderText({
    ""
  })

  observe({
    val <- bracketVal2()[2]
    val2 <- bracketVal3()[1]

    if (val2 < val) {
      output$warn2 <- renderText({
        "Warning: Second and third brackets overlap."
      })
    }
    if (val2 == val) {
      output$warn2 <- renderText({
        ""
      })
    }

    if (val2 > val) {
      output$warn2 <- renderText({
        "Warning: Gap between second and third brackets."
      })
    }
  })

  observe({
    val <- bracketVal3()[2]
    updateSliderInput(session, "bracketV4", value = val)
    output$warn3 <- renderText({
      ""
    })
  })
  observe({
    val <- bracketVal3()[2]
    val2 <- bracketVal4()

    if (val2 < val) {
      output$warn3 <- renderText({
        "Warning: Third and fourth brackets overlap."
      })
    }
    if (val2 == val) {
      output$warn3 <- renderText({
        ""
      })
    }

    if (val2 > val) {
      output$warn3 <- renderText({
        "Warning: Gap between third and fourth brackets."
      })
    }
  })

  ### marginal tax rate only increasing
  observe({
    if (bracket2() < bracket1()) {
      updateSliderInput(session, "bracket2",
        min = 0, value = bracket1(),
        max = 10, step = 1
      )
    }
  })

  observe({
    if (bracket3() < bracket2()) {
      updateSliderInput(session, "bracket3",
        min = 0, value = bracket2(),
        max = 10, step = 1
      )
    }
  })

  observe({
    if (bracket4() < bracket3()) {
      updateSliderInput(session, "bracket4",
        min = 0, value = bracket3(),
        max = 10, step = 1
      )
    }
  })

  ## streamline the input references
  bracket1 <- reactive({
    input$bracket1
  })
  bracket2 <- reactive({
    input$bracket2
  })
  bracket3 <- reactive({
    input$bracket3
  })
  bracket4 <- reactive({
    input$bracket4
  })
  bracket5 <- reactive({
    input$bracket5
  })



  bracketVal1 <- reactive({
    input$bracketV1
  })
  bracketVal2 <- reactive({
    input$bracketV2
  })
  bracketVal3 <- reactive({
    input$bracketV3
  })
  bracketVal4 <- reactive({
    input$bracketV4
  })

  dataInput <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)


    xval <- seq(10e6, 10e9 + 1e8, by = 5e6)


    idx1 <- xval <= bracketVal1()[2] * 1e6
    idx2 <- xval > bracketVal2()[1] * 1e6 & xval <= bracketVal2()[2] * 1e6
    idx3 <- xval > bracketVal3()[1] * 1e6 & xval <= bracketVal3()[2] * 1e6
    idx4 <- xval > bracketVal4() * 1e6




    idx <- cbind.data.frame(idx1, idx2, idx3, idx4)


    # Indicator across income on tax bracke position
    getGroup <- unlist(apply(idx, 1, function(x) {
      which(x)[1]
    }))

    toPlot <- cbind.data.frame(xval, getGroup)

    toMatch <- cbind.data.frame(group = 1:4, tax = taxRate)

    toPlot2 <- merge(toPlot, toMatch, by.x = "getGroup", by.y = "group")

    toPlot2$marginalInt <- unlist(lapply(toPlot2$xval, getAverageTax, taxRate))

    toPlot2$marginalRate <- (toPlot2$marginalInt / toPlot2$xval) * 100


    toPlot2$id <- 1:nrow(toPlot2)

    toPlot2
  })


  # Computes total tax revenue
  getAverageTax <- function(wealth, taxLevels) {
    ## expecting taxLevels in percentage

    taxLevels <- taxLevels / 100

    first <- wealth - input$bracketV1[1] * 1e6
    second <- first - (input$bracketV1[2] * 1e6 - input$bracketV1[1] * 1e6)
    third <- second - (input$bracketV2[2] * 1e6 - input$bracketV2[1] * 1e6)
    fourth <- third - (input$bracketV3[2] * 1e6 - input$bracketV3[1] * 1e6)


    firstChunk <- ifelse(second >= 0, taxLevels[1] * (input$bracketV1[2] * 1e6 - input$bracketV1[1] * 1e6), taxLevels[1] * first)
    secondChunk <- ifelse(second >= 0, taxLevels[2] * (input$bracketV2[2] * 1e6 - input$bracketV2[1] * 1e6), taxLevels[2] * max(second, 0))
    thirdChunk <- ifelse(third >= 0, taxLevels[3] * (input$bracketV3[2] * 1e6 - input$bracketV3[1] * 1e6), taxLevels[3] * max(third, 0))
    fourthChunk <- ifelse(fourth >= 0, fourth * taxLevels[4], 0)

    toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk
    return(toReturn)
  }

  totalTax <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
    taxRateP <- taxRate / 100 ## get to percentage
    bracketStarts <- 1e6 * c(input$bracketV1[1], input$bracketV2[1], input$bracketV3[1], input$bracketV4[1])
    taxPerBracket <- getTaxBasePerBracket(grid, bracketStarts)
    taxBase <- taxPerBracket$taxBase / 1e9 ## in billions
    tax <- taxBase * taxRateP

    totalTax <- sum(tax)
  })

  output$totalTax <- renderText({
    round(totalTax())
  })

  output$totalTax_10 <- renderText({
    totalTax10 <- totalTax() * 13

    round(totalTax10 / 1e3, 2)
  })

  householdsTaxed <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
    taxRateP <- taxRate / 100 ## get to percentage

    bracketStarts <- 1e6 * c(input$bracketV1[1], input$bracketV2[1], input$bracketV3[1], input$bracketV4[1])
    peoplePerBracket <- getPeoplePerBracket(grid, bracketStarts)
    numberTaxpayers <- peoplePerBracket$totalPeople
    householdsTaxed <- numberTaxpayers * (taxRateP > 0)
    householdsTaxed
  })

  output$totalTaxpayers <- renderText({
    totalTaxpayers <- sum(householdsTaxed())

    round(totalTaxpayers)
  })

  output$percentHouseAffected <- renderText({
    householdsAffected <- sum(householdsTaxed()) / 129.4e6 ## make the denominator updateable later

    round(householdsAffected * 100, 1) ## get to percentage
  })

  output$percentTaxUnits <- renderText({
    taxUnits <- sum(householdsTaxed()) / 183460000 ## make the denominator updateable later

    round(taxUnits * 100, 2) ## get to percentage
  })


  vis2 <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)

    # These are mini data set that ggvis needs to create vertical lines
    extra0 <- cbind.data.frame(x = rep(bracketVal1()[1] * 1e6, 2), y = c(0, taxRate[1]))
    extra1 <- cbind.data.frame(x = rep(bracketVal1()[2] * 1e6, 2), y = c(0, taxRate[1]))
    extra1b <- cbind.data.frame(x = rep(bracketVal2()[1] * 1e6, 2), y = c(0, taxRate[2]))
    extra2 <- cbind.data.frame(x = rep(bracketVal2()[2] * 1e6, 2), y = c(0, taxRate[2]))
    extra2b <- cbind.data.frame(x = rep(bracketVal3()[1] * 1e6, 2), y = c(0, taxRate[3]))

    extra3 <- cbind.data.frame(x = rep(bracketVal3()[2] * 1e6, 2), y = c(0, taxRate[3]))
    extra3b <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[4]))


    showMargin <- function(x) {

      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      if (is.null(x)) return(NULL)
      data <- subset(dataInput(), xval < 1e9)
      row <- data[data$id == x$id, ]
      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", sep = "")
    }

    subset(dataInput(), xval < 1e9) %>%
      ggvis(x = ~xval, y = ~tax) %>%
      layer_points() %>%
      layer_points(x = ~xval, y = ~marginalRate, stroke := "red", key := ~id) %>%
      add_tooltip(showMargin, "hover") %>%
      layer_lines(x = ~xval, y = ~marginalRate, stroke := "red") %>%
      layer_paths(data = extra1, ~x, ~y) %>%
      layer_paths(data = extra2, ~x, ~y) %>%
      layer_paths(data = extra3, ~x, ~y) %>%
      layer_paths(data = extra0, ~x, ~y) %>%
      layer_paths(data = extra1b, ~x, ~y) %>%
      layer_paths(data = extra2b, ~x, ~y) %>%
      layer_paths(data = extra3b, ~x, ~y) %>%
      add_axis("x", title = "Wealth before taxes") %>%
      add_axis("y", title = "Tax rate (%)") %>%
      set_options(width = 1000, height = 500)
  })


  vis2 %>% bind_shiny("plot2")


  visBillion <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)

    # These are mini data set that ggvis needs to create vertical lines
    extra0 <- cbind.data.frame(x = rep(bracketVal1()[1] * 1e6, 2), y = c(0, taxRate[1]))
    extra1 <- cbind.data.frame(x = rep(bracketVal1()[2] * 1e6, 2), y = c(0, taxRate[1]))
    extra1b <- cbind.data.frame(x = rep(bracketVal2()[1] * 1e6, 2), y = c(0, taxRate[2]))
    extra2 <- cbind.data.frame(x = rep(bracketVal2()[2] * 1e6, 2), y = c(0, taxRate[2]))
    extra2b <- cbind.data.frame(x = rep(bracketVal3()[1] * 1e6, 2), y = c(0, taxRate[3]))

    extra3 <- cbind.data.frame(x = rep(bracketVal3()[2] * 1e6, 2), y = c(0, taxRate[3]))
    extra3b <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[4]))

    showMargin <- function(x) {
      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      if (is.null(x)) return(NULL)
      data <- subset(dataInput(), xval >= 1e9)
      row <- data[data$id == x$id, ]
      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", sep = "")
    }

    subset(dataInput(), xval >= 1e9) %>%
      ggvis(x = ~xval, y = ~tax) %>%
      layer_points() %>%
      layer_points(x = ~xval, y = ~marginalRate, stroke := "red", key := ~id) %>%
      add_tooltip(showMargin, "hover") %>%
      layer_lines(x = ~xval, y = ~marginalRate, stroke := "red") %>%
      layer_paths(data = extra3, ~x, ~y) %>%
      layer_paths(data = extra3b, ~x, ~y) %>%
      add_axis("x", title = "Wealth before taxes") %>%
      add_axis("y", title = "Tax rate (%)") %>%
      scale_numeric("x", domain = c(1e9, 10e9), nice = FALSE) %>%
      set_options(width = 1000, height = 500)
  })

  visBillion %>% bind_shiny("plotB")
}