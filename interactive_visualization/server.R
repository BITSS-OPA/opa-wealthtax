
server <- function(input, output, session) {

  grid <- read.csv("taxBaseGridUpdated.csv")
  grid$thres <- (1 - .16) * grid$thres ## evasion, this parameter will be tunable later on
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
    val <- bracketVal1T()
    # val2 <- bracketVal2() ## avoid switching back and forth
    val2 <- as.numeric(input$bracketV2T)
    updateTextInput(session, "bracketV2T",value=val2)
    
  })

  observe({
    val <- bracketVal2T()
    # val2 <- bracketVal3() ## avoid switching back and forth
    val2 <- as.numeric(input$bracketV3T)

    updateTextInput(session, "bracketV3T",value=val2)
    
  })

  observe({
    val <- bracketVal3T()
    # val2 <- bracketVal4() ## avoid switching back and forth
    val2 <- as.numeric(input$bracketV4T)

    updateTextInput(session, "bracketV4T",value=val2)
    
  })

  observe({
    #if (input$extraBrackets>=1) {
      val <- bracketVal4T()
      val2 <- as.numeric(input$bracketV5T)

      updateTextInput(session, "bracketV5T",value=val2)
      
    #}
  })

  observe({
    #if (input$extraBrackets>=2) {
      val <- as.numeric(bracketVal5T())
      val2 <- as.numeric(input$bracketV6T)

      updateTextInput(session, "bracketV6T",value=val2)
      
   # }
  })

  observe({
    #if (input$extraBrackets>=3) {
      val <- as.numeric(bracketVal6T())
      val2 <- as.numeric(input$bracketV7T)

      updateTextInput(session, "bracketV7T",value=val2)
      
    #}
  })

  observe({
    #if (input$extraBrackets>=4) {
      val <- as.numeric(bracketVal7T())
      val2 <- as.numeric(input$bracketV8T)

      updateTextInput(session, "bracketV8T",value=val2)
      
    #}
  })

  ###

  ### marginal tax rate only increasing
  observe({
    if (bracket2T() < bracket1T()) {
      updateTextInput(session, "bracket2T",value=bracket1T())
    }
  })

  observe({
    if (bracket3T() < bracket2T()) {
      updateTextInput(session, "bracket3T",value=bracket2T())
      
    }
  })

  observe({
    if (bracket4T() < bracket3T()) {
      updateTextInput(session, "bracket4T",value=bracket3T())
      
    }
  })

  observe({
    #if (input$extraBrackets>=1) {
      if (bracket5T() < bracket4T()) {
        updateTextInput(session, "bracket5T",value=bracket4T())
        
      }
    #}
  })

  observe({
    #if (input$extraBrackets>=2) {
      if (bracket6T() < bracket5T()) {
        updateTextInput(session, "bracket6T",value=bracket5T())
        
      }
    #}
  })

  observe({
    #if (input$extraBrackets>=3) {
      if (bracket7T() < bracket6T()) {
        updateTextInput(session, "bracket7T",value=bracket6T())
        
      }
    #}
  })

  observe({
    #if (input$extraBrackets>=4) {
      if (bracket8T() < bracket7T()) {
        updateTextInput(session, "bracket8T",value=bracket7T())
        
      }
    #}
  })


  
  observe({
   
      updateTextInput(session, "bracket5T",value=bracket5T())
      
  
  })

  bracket1T <- reactive({
    as.numeric(input$bracket1T)
    # print(input$bracket1) ## make sure doesn't have % included
  })
  bracket2T <- reactive({
    as.numeric(input$bracket2T)
  })
  bracket3T <- reactive({
    as.numeric(input$bracket3T)
  })
  bracket4T <- reactive({
    as.numeric(input$bracket4T)
  })
  bracket5T <- reactive({
    as.numeric(input$bracket5T)
  })
  bracket6T <- reactive({
    as.numeric(input$bracket6T)
  })
  bracket7T <- reactive({
    as.numeric(input$bracket7T)
  })
  bracket8T <- reactive({
    as.numeric(input$bracket8T)
  })

  bracketVal1T <- reactive({
    as.numeric(input$bracketV1T)
  })
  bracketVal2T <- reactive({
    as.numeric(input$bracketV2T)
  })
  bracketVal3T <- reactive({
    as.numeric(input$bracketV3T)
  })
  bracketVal4T <- reactive({
    as.numeric(input$bracketV4T)
  })
  bracketVal5T <- reactive({
    as.numeric(input$bracketV5T)
  })
  bracketVal6T <- reactive({
    as.numeric(input$bracketV6T)
  })
  bracketVal7T <- reactive({
    as.numeric(input$bracketV7T)
  })
  bracketVal8T <- reactive({
    as.numeric(input$bracketV8T)
  })





  dataInputT <- reactive({

    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))

    if (input$extraBrackets==1) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T))
    }
    if (input$extraBrackets==2) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
    }
    if (input$extraBrackets==3) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T))
    }
    if (input$extraBrackets==4) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T), as.numeric(input$bracket8T))
    }
print(taxRate)

    xval <- 10^seq(log10(10e6), log10(45e9), by = 0.001) ## get uniform on log scale


    idx1 <- xval <= as.numeric(bracketVal2T()) * 1e6
    idx2 <- xval > as.numeric(bracketVal2T()) * 1e6 & xval <= as.numeric(bracketVal3T()) * 1e6
    idx3 <- xval > as.numeric(bracketVal3T()) * 1e6 & xval <= as.numeric(bracketVal4T()) * 1e6

    if (input$extraBrackets==4) { ## since nested, test this one first
      idx4 <- xval > as.numeric(bracketVal4T()) * 1e6 & xval <= as.numeric(input$bracketV5T) * 1e6
      idx5 <- xval > as.numeric(input$bracketV5T) * 1e6 & xval <= as.numeric(input$bracketV6T) * 1e6
      idx6 <- xval > as.numeric(input$bracketV6T) * 1e6 & xval <= as.numeric(input$bracketV7T) * 1e6
      idx7 <- xval > as.numeric(input$bracketV7T) * 1e6 & xval <= as.numeric(input$bracketV8T) * 1e6
      idx8 <- xval > as.numeric(input$bracketV8T)
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8)
    } else if (input$extraBrackets==3) {
      idx4 <- xval > as.numeric(bracketVal4T()) * 1e6 & xval <= as.numeric(input$bracketV5T) * 1e6
      idx5 <- xval > as.numeric(input$bracketV5T) * 1e6 & xval <= as.numeric(input$bracketV6T) * 1e6
      idx6 <- xval > as.numeric(input$bracketV6T) * 1e6 & xval <= as.numeric(input$bracketV7T) * 1e6
      idx7 <- xval > as.numeric(input$bracketV7T) * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7)
    } else if (input$extraBrackets==2) {
      idx4 <- xval > as.numeric(bracketVal4T()) * 1e6 & xval <= as.numeric(input$bracketV5T) * 1e6
      idx5 <- xval > as.numeric(input$bracketV5T) * 1e6 & xval <= as.numeric(input$bracketV6T) * 1e6
      idx6 <- xval > as.numeric(input$bracketV6T) * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6)
    } else if (input$extraBrackets==1) {
      idx4 <- xval > as.numeric(bracketVal4T()) * 1e6 & xval <= as.numeric(input$bracketV5T) * 1e6
      idx5 <- xval > as.numeric(input$bracketV5T) * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5)
    } else {
      idx4 <- xval > as.numeric(bracketVal4T()) * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4)
    }



    # Indicator across income on tax bracke position
    getGroup <- unlist(apply(idx, 1, function(x) {
      which(x)[1]
    }))
    # getGroup <- as.numeric(cut(xval, c(brackets_po, 1e12), include.lowest = TRUE))


    toPlot <- cbind.data.frame(xval, getGroup)


    toMatch <- cbind.data.frame(group = 1:length(taxRate), tax = taxRate)

    toPlot2 <- merge(toPlot, toMatch, by.x = "getGroup", by.y = "group")

    brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    if (input$extraBrackets==1) {
      brackets <- c(brackets, as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets==2) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets==3) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T),as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets==4) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T),as.numeric(input$bracketV7T), as.numeric(input$bracketV8T))
    }


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
    if (length(taxLevels) == 5) {
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)
    }
    if (length(taxLevels) == 6) {
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
    }
    if (length(taxLevels) == 7) {
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
      seventh <- sixth - (brackets[7] * 1e6 - brackets[6] * 1e6)
    }
    if (length(taxLevels) == 8) {
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
      seventh <- sixth - (brackets[7] * 1e6 - brackets[6] * 1e6)
      eighth <- seventh - (brackets[8] * 1e6 - brackets[7] * 1e6)
    }

    firstChunk <- ifelse(second >= 0, taxLevels[1] * (brackets[2] * 1e6 - brackets[1] * 1e6), taxLevels[1] * max(first, 0))
    secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
    thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))

    if (length(taxLevels) == 8) {
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(seventh >= 0, taxLevels[6] * (brackets[7] * 1e6 - brackets[6] * 1e6), taxLevels[6] * max(sixth, 0))
      seventhChunk <- ifelse(eighth >= 0, taxLevels[7] * (brackets[8] * 1e6 - brackets[7] * 1e6), taxLevels[7] * max(seventh, 0))
      eighthChunk <- ifelse(eighth >= 0, eighth * taxLevels[8], 0)

      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk + seventhChunk + eighthChunk
    } else if (length(taxLevels) == 7) {
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(seventh >= 0, taxLevels[6] * (brackets[7] * 1e6 - brackets[6] * 1e6), taxLevels[6] * max(sixth, 0))
      seventhChunk <- ifelse(seventh >= 0, seventh * taxLevels[7], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk + seventhChunk
    } else if (length(taxLevels) == 6) {
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(sixth >= 0, sixth * taxLevels[6], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk
    } else if (length(taxLevels) == 5) {
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(fifth >= 0, fifth * taxLevels[5], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk
    } else {
      fourthChunk <- ifelse(fourth >= 0, fourth * taxLevels[4], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk
    }
    return(toReturn)
  }

  totalTax <- reactive({
    
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
      if (input$extraBrackets==1) {
        taxRate <- c(taxRate, input$bracket5T)
      }
      if (input$extraBrackets==2) {
        taxRate <- c(taxRate,  input$bracket5T, input$bracket6T)
      }
      if (input$extraBrackets==3) {
        taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T)
      }
      if (input$extraBrackets==4) {
        taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T, input$bracket8T)
      }
      taxRateP <- as.numeric(taxRate) / 100 ## get to percentage
      bracketStarts <- 1e6 * as.numeric(c(input$bracketV1T, input$bracketV2T, input$bracketV3T, input$bracketV4T))
      if (input$extraBrackets==1) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T))
      }
      if (input$extraBrackets==2) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T))
      }
      if (input$extraBrackets==3) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T),1e6 * as.numeric(input$bracketV7T))
      }
      if (input$extraBrackets==4) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T),1e6 * as.numeric(input$bracketV7T),1e6 * as.numeric(input$bracketV8T))
      }
    
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
    
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
      if (input$extraBrackets==1) {
        taxRate <- c(taxRate, input$bracket5T)
      }
      if (input$extraBrackets==2) {
        taxRate <- c(taxRate, input$bracket5T, input$bracket6T)
      }
      if (input$extraBrackets==3) {
        taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T)
      }
      if (input$extraBrackets==4) {
        taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T,input$bracket8T)
      }
      taxRateP <- as.numeric(taxRate) / 100 ## get to percentage

      bracketStarts <- 1e6 * as.numeric(c(input$bracketV1T, input$bracketV2T, input$bracketV3T, input$bracketV4T))
      if (input$extraBrackets==1) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T))
      }
      if (input$extraBrackets==2) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T),1e6 * as.numeric(input$bracketV6T))
      }
      if (input$extraBrackets==3) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T),1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T))
      }
      if (input$extraBrackets==4) {
        bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T),1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T), 1e6 * as.numeric(input$bracketV8T))
      }
    
    peoplePerBracket <- getPeoplePerBracket(grid, bracketStarts)
    numberTaxpayers <- peoplePerBracket$totalPeople
    householdsTaxed <- numberTaxpayers * (taxRateP > 0)
    householdsTaxed
  })

  output$totalTaxpayers <- renderText({
    totalTaxpayers <- sum(householdsTaxed())

    round(totalTaxpayers)
  })

  # output$percentHouseAffected <- renderText({
  #   #browser()
  #   householdsAffected <- sum(householdsTaxed()) / 129.4e6 ## make the denominator updateable later
  #
  #   round(householdsAffected * 100, 1) ## get to percentage
  # })

  output$percentTaxUnits <- renderText({
    taxUnits <- sum(householdsTaxed()) / sum(grid$nb)
    ## double check

    round(taxUnits * 100, 2) ## get to percentage
  })


  vis2 <- reactive({
    
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
      if (input$extraBrackets==1) {
        taxRate <- c(taxRate, as.numeric(input$bracket5T))
      }
      if (input$extraBrackets==2) {
        taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
      }
      if (input$extraBrackets==3) {
        taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T),as.numeric(input$bracket7T))
      }
      if (input$extraBrackets==4) {
        taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T),as.numeric(input$bracket7T),as.numeric(input$bracket8T))
      }

      # These are mini data set that ggvis needs to create vertical lines
      extra0 <- cbind.data.frame(x = rep(as.numeric(bracketVal1T()) * 1e6, 2), y = c(0, taxRate[1]))
      extra1 <- cbind.data.frame(x = rep(as.numeric(bracketVal2T()) * 1e6, 2), y = c(0, taxRate[1]))
      extra1b <- cbind.data.frame(x = rep(as.numeric(bracketVal2T()) * 1e6, 2), y = c(0, taxRate[2]))
      extra2 <- cbind.data.frame(x = rep(as.numeric(bracketVal3T()) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(bracketVal3T()) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(bracketVal4T()) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(bracketVal4T()) * 1e6, 2), y = c(0, taxRate[4]))
      if (input$extraBrackets==1) {
        extra4 <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[5]))
      }
      if (input$extraBrackets==2) {
        extra4 <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5 <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5b <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[6]))
      }
      if (input$extraBrackets==3) {
        extra4 <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5 <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5b <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[6]))
        extra6 <- cbind.data.frame(x = rep(as.numeric(input$bracketV7T) * 1e6, 2), y = c(0, taxRate[6]))
        extra6b <- cbind.data.frame(x = rep(as.numeric(input$bracketV7T) * 1e6, 2), y = c(0, taxRate[7]))
      }
      if (input$extraBrackets==4) {
        extra4 <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(as.numeric(input$bracketV5T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5 <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[5]))
        extra5b <- cbind.data.frame(x = rep(as.numeric(input$bracketV6T) * 1e6, 2), y = c(0, taxRate[6]))
        extra6 <- cbind.data.frame(x = rep(as.numeric(input$bracketV7T) * 1e6, 2), y = c(0, taxRate[6]))
        extra6b <- cbind.data.frame(x = rep(as.numeric(input$bracketV7T) * 1e6, 2), y = c(0, taxRate[7]))
        extra7 <- cbind.data.frame(x = rep(as.numeric(input$bracket8T) * 1e6, 2), y = c(0, taxRate[7]))
        extra7b <- cbind.data.frame(x = rep(as.numeric(input$bracketV8T) * 1e6, 2), y = c(0, taxRate[8]))
      }
    

    ## rename to showAvg

    showAvg <- function(x) {
      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      # https://stackoverflow.com/questions/31230124/exclude-line-points-from-showing-info-when-using-add-tooltip-with-hover-in-ggvis
      if (sum(grepl("id", names(x))) == 0) return(NULL)
      if (is.null(x)) return(NULL)

      
        data <- dataInputT()



      row <- data[data$id == x$id, ]

      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", " <br> Wealth ($m): ", round(row$xval / 1e6, 0), "<br> Taxes Paid ($m) ", round(row$marginalInt / 1e6, 2), sep = "") ## dividing by 1e6 may need to change if we do this for xval overall
    }

    # plot <- dataInput()[, -ncol(dataInput())] %>%

    
      data <- dataInputT()


    rmIdx <- ncol(data)
    plot <- data[, -rmIdx] %>%
      ggvis(x = ~ xval / 1e6, y = ~tax) %>%
      layer_points() %>%
      layer_points(data = data, x = ~ xval / 1e6, y = ~marginalRate, stroke := "red", key := ~id) %>%
      add_tooltip(showAvg, "hover") %>%
      layer_lines(x = ~ xval / 1e6, y = ~marginalRate, stroke := "red") %>%
      layer_paths(data = extra1, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra0, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra1b, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
      add_axis("x", title_offset = 80, title = "Wealth ($m)", grid = F, format = ",", properties = axis_props(labels = list(angle = 45, align = "left", baseline = "middle"))) %>%
      add_axis("y", title = "Tax rate (%)") %>%
      scale_numeric("x", trans = "log", expand = 0) %>%
      set_options(width = 1000, height = 500)
    if (input$extraBrackets==4) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets==3) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets==2) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets==1) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y)
    } else {
      plot
    }
  })


  vis2 %>% bind_shiny("plot2")
}