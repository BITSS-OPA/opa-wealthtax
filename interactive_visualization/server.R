
server <- function(input, output, session) {
  # grid <- read.csv("taxBaseGrid.csv")
  grid <- read.csv("taxBaseGridUpdated.csv")
  # print(head(grid)) ## check that app has access to this file

  getTaxBasePerBracket <- function(grid, brackets) {
    ## brackets is lower end of each bracket
    brackets <- c(brackets, 1e10 + 1e6) ## get last bracket
    grid$total <- grid$nb * grid$avg
    grid$group <- cut(grid$thres, brackets)
    toReturn <- grid %>% group_by(group) %>% summarise(taxBase = sum(total)) %>% drop_na() %>% complete(group, fill = list(taxBase = 0)) ## avoid dropping levels without any taxBase

    # https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in

    return(toReturn)
  }

  getPeoplePerBracket <- function(grid, brackets) {
    ## brackets is lower end of each bracket
    brackets <- c(brackets, 1e10 + 1e6) ## get last bracket
    grid$group <- cut(grid$thres, brackets, include.lowest = T)
    toReturn <- grid %>% group_by(group) %>% summarise(totalPeople = sum(nb)) %>% drop_na() %>% complete(group, fill = list(totalPeople = 0)) ## avoid dropping levels without any people

    # https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in

    return(toReturn)
  }

  ### update tax brackets based on previous decisions
  observe({
    val <- bracketVal1()
    # val2 <- bracketVal2() ## avoid switching back and forth
    val2 <- input$bracketV2
    updateSliderInput(session, "bracketV2",
      min = val + 5, value = ifelse(val2 > val, val2, val + 50),
      max = 1000, step = 5
    )
  })

  observe({
    val <- bracketVal2()
    # val2 <- bracketVal3() ## avoid switching back and forth
    val2 <- input$bracketV3

    updateSliderInput(session, "bracketV3", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
  })

  observe({
    val <- bracketVal3()
    # val2 <- bracketVal4() ## avoid switching back and forth
    val2 <- input$bracketV4

    updateSliderInput(session, "bracketV4", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
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
    # print(input$bracket1) ## make sure doesn't have % included
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


    xval <- 10^seq(log10(10e6), log10(45e9), by = 0.001) ## get uniform on log scale


    idx1 <- xval <= bracketVal2() * 1e6
    idx2 <- xval > bracketVal2() * 1e6 & xval <= bracketVal3() * 1e6
    idx3 <- xval > bracketVal3() * 1e6 & xval <= bracketVal4() * 1e6
    idx4 <- xval > bracketVal4() * 1e6


    idx <- cbind.data.frame(idx1, idx2, idx3, idx4)


    # Indicator across income on tax bracke position
    getGroup <- unlist(apply(idx, 1, function(x) {
      which(x)[1]
    }))
    #getGroup <- as.numeric(cut(xval, c(brackets_po, 1e12), include.lowest = TRUE))


    toPlot <- cbind.data.frame(xval, getGroup)

    toMatch <- cbind.data.frame(group = 1:4, tax = taxRate)

    toPlot2 <- merge(toPlot, toMatch, by.x = "getGroup", by.y = "group")

    brackets <- c(bracketVal1(), bracketVal2(), bracketVal3(), bracketVal4())

    toPlot2$marginalInt <- unlist(lapply(toPlot2$xval, getAverageTax, taxRate, brackets))

    toPlot2$marginalRate <- (toPlot2$marginalInt / toPlot2$xval) * 100


    toPlot2$id <- 1:nrow(toPlot2)

    toPlot2
  })


  # Computes total tax revenue
  getAverageTax <- function(wealth, taxLevels, brackets) {
    ## pass in brackets to make sure they update
    ## expecting taxLevels in percentage
    taxLevels <- taxLevels / 100
    first <- wealth - brackets[1] * 1e6
    second <- first - (brackets[2] * 1e6 - brackets[1] * 1e6)
    third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
    fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)

    firstChunk <- ifelse(second >= 0, taxLevels[1] * (brackets[2] * 1e6 - brackets[1] * 1e6), taxLevels[1] * max(first, 0))
    secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
    thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
    fourthChunk <- ifelse(fourth >= 0, fourth * taxLevels[4], 0)

    toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk
    return(toReturn)
  }

  totalTax <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
    taxRateP <- taxRate / 100 ## get to percentage
    bracketStarts <- 1e6 * c(input$bracketV1, input$bracketV2, input$bracketV3, input$bracketV4)
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

    bracketStarts <- 1e6 * c(input$bracketV1, input$bracketV2, input$bracketV3, input$bracketV4)
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
    extra0 <- cbind.data.frame(x = rep(bracketVal1() * 1e6, 2), y = c(0, taxRate[1]))
    extra1 <- cbind.data.frame(x = rep(bracketVal2() * 1e6, 2), y = c(0, taxRate[1]))
    extra1b <- cbind.data.frame(x = rep(bracketVal2() * 1e6, 2), y = c(0, taxRate[2]))
    extra2 <- cbind.data.frame(x = rep(bracketVal3() * 1e6, 2), y = c(0, taxRate[2]))
    extra2b <- cbind.data.frame(x = rep(bracketVal3() * 1e6, 2), y = c(0, taxRate[3]))
    extra3 <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[3]))
    extra3b <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[4]))

## rename to showAvg
    #https://stackoverflow.com/questions/31230124/exclude-line-points-from-showing-info-when-using-add-tooltip-with-hover-in-ggvis
    showMargin <- function(x) {
      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      if (is.null(x)) return(NULL)
      data <- dataInput()
      data$id=1:nrow(data)
      print(head(x))
      idx = order(abs(data$id-x$id))
      row1 <- data[idx[1],]
      row2 <- data[idx[2],]

      #row <- data[data$id == x$id, ]
      val <- mean(c(row1$marginalRate,row2$marginalRate),na.rm=T)
     #print(x) ## sometimes missing id
      paste0("Average Tax Rate: ", round(val, 2), "%", sep = "")
    }
#browser()
    dataInput() %>%
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
      add_axis("x", title_offset=80, title = "Wealth before taxes",grid=F,format=",",properties = axis_props(labels = list(angle = 45,  align = "left", baseline = "middle"))) %>%
      add_axis("y", title = "Tax rate (%)") %>%
      scale_numeric("x", trans = "log", expand = 0) %>%
      set_options(width = 1000, height = 500)
  })


  vis2 %>% bind_shiny("plot2")
}