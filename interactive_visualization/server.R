
server <- function(input, output, session) {
  #browser()
  # grid <- read.csv("taxBaseGrid.csv")
  grid <- read.csv("taxBaseGridUpdated.csv")
  # print(head(grid)) ## check that app has access to this file
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
  
  observe({
    if (input$extraBracket1){
    val <- bracketVal4()
    val2 <- input$bracketV5
    
    updateSliderInput(session, "bracketV5", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    }
  })
  
  observe({
    if (input$extraBracket2){
      val <- input$bracketV5
      val2 <- input$bracketV6
      
      updateSliderInput(session, "bracketV6", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    }
  })
  
  observe({
    if (input$extraBracket3){
      val <- input$bracketV6
      val2 <- input$bracketV7
      
      updateSliderInput(session, "bracketV7", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    }
  })
  
  observe({
    if (input$extraBracket4){
      val <- input$bracketV7
      val2 <- input$bracketV8
      
      updateSliderInput(session, "bracketV8", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    }
  })
  

  ### marginal tax rate only increasing
  observe({
    if (bracket2() < bracket1()) {
      updateSliderInput(session, "bracket2",
        min = 0, value = bracket1(),
        max = 10
      )
    }
  })

  observe({
    if (bracket3() < bracket2()) {
      updateSliderInput(session, "bracket3",
        min = 0, value = bracket2(),
        max = 10
      )
    }
  })

  observe({
    if (bracket4() < bracket3()) {
      updateSliderInput(session, "bracket4",
        min = 0, value = bracket3(),
        max = 10
      )
    }
  })
  
  observe({
    if (input$extraBracket1){
      if(bracket5()< bracket4()){
        updateSliderInput(session, "bracket5",
                          min = 0, value = bracket4(),
                          max = 10)
      }
    }
  })

  observe({
    if (input$extraBracket2){
      if(bracket6()< bracket5()){
        updateSliderInput(session, "bracket6",
                          min = 0, value = bracket5(),
                          max = 10)
      }
    }
  })
  
  observe({
    if (input$extraBracket3){
      if(bracket7()< bracket6()){
        updateSliderInput(session, "bracket7",
                          min = 0, value = bracket6(),
                          max = 10)
      }
    }
  })
  
  observe({
    if (input$extraBracket4){
      if(bracket8()< bracket7()){
        updateSliderInput(session, "bracket8",
                          min = 0, value = bracket7(),
                          max = 10)
      }
    }
  })
 
    ### update tax brackets based on previous decisions
    observe({
      val <- bracketVal1T()
      # val2 <- bracketVal2() ## avoid switching back and forth
      val2 <- input$bracketV2T
      updateSliderInput(session, "bracketV2T",
                        min = val + 5, value = ifelse(val2 > val, val2, val + 50),
                        max = 1000, step = 5
      )
    })
    
    observe({
      val <- bracketVal2T()
      # val2 <- bracketVal3() ## avoid switching back and forth
      val2 <- input$bracketV3T
      
      updateSliderInput(session, "bracketV3T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    })
    
    observe({
      val <- bracketVal3T()
      # val2 <- bracketVal4() ## avoid switching back and forth
      val2 <- input$bracketV4T
      
      updateSliderInput(session, "bracketV4T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
    })
    
    observe({
      if (input$extraBracket1){
        val <- bracketVal4T()
        val2 <- input$bracketV5T
        
        updateSliderInput(session, "bracketV5T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
      }
    })
    
    observe({
      if (input$extraBracket2){
        val <- input$bracketV5T
        val2 <- input$bracketV6T
        
        updateSliderInput(session, "bracketV6T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
      }
    })
    
    observe({
      if (input$extraBracket3){
        val <- input$bracketV6T
        val2 <- input$bracketV7T
        
        updateSliderInput(session, "bracketV7T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
      }
    })
    
    observe({
      if (input$extraBracket4){
        val <- input$bracketV7T
        val2 <- input$bracketV8T
        
        updateSliderInput(session, "bracketV8T", min = val + 5, value = ifelse(val2 > val, val2, val + 50))
      }
    })
    
    
    ### marginal tax rate only increasing
    observe({
      if (bracket2T() < bracket1T()) {
        updateSliderInput(session, "bracket2T",
                          min = 0, value = bracket1T(),
                          max = 10
        )
      }
    })
    
    observe({
      if (bracket3T() < bracket2T()) {
        updateSliderInput(session, "bracket3T",
                          min = 0, value = bracket2T(),
                          max = 10
        )
      }
    })
    
    observe({
      if (bracket4T() < bracket3T()) {
        updateSliderInput(session, "bracket4T",
                          min = 0, value = bracket3T(),
                          max = 10
        )
      }
    })
    
    observe({
      if (input$extraBracket1){
        if(bracket5T()< bracket4T()){
          updateSliderInput(session, "bracket5T",
                            min = 0, value = bracket4T(),
                            max = 10)
        }
      }
    })
    
    observe({
      if (input$extraBracket2){
        if(bracket6T()< bracket5T()){
          updateSliderInput(session, "bracket6T",
                            min = 0, value = bracket5T(),
                            max = 10)
        }
      }
    })
    
    observe({
      if (input$extraBracket3){
        if(bracket7T()< bracket6T()){
          updateSliderInput(session, "bracket7T",
                            min = 0, value = bracket6T(),
                            max = 10)
        }
      }
    })
    
    observe({
      if (input$extraBracket4){
        if(bracket8T()< bracket7T()){
          updateSliderInput(session, "bracket8T",
                            min = 0, value = bracket7T(),
                            max = 10)
        }
      }
    })
    
 
  ## streamline the input references
  bracket1 <- reactive({
    as.numeric(input$bracket1)
     #print(input$bracket1) ## make sure doesn't have % included
 
  })
  bracket2 <- reactive({
    as.numeric(input$bracket2)
  })
  bracket3 <- reactive({
    as.numeric(input$bracket3)
  })
  bracket4 <- reactive({
    as.numeric(input$bracket4)
  })
  bracket5 <- reactive({
    as.numeric(input$bracket5)
  })
  bracket6 <- reactive({
    as.numeric(input$bracket6)
  })
  bracket7 <- reactive({
    as.numeric(input$bracket7)
  })
  bracket8 <- reactive({
    as.numeric(input$bracket8)
  })

  bracketVal1 <- reactive({
    as.numeric(input$bracketV1)
  })
  bracketVal2 <- reactive({
    as.numeric(input$bracketV2)
  })
  bracketVal3 <- reactive({
    as.numeric(input$bracketV3)
  })
  bracketVal4 <- reactive({
    as.numeric(input$bracketV4)
  })
  bracketVal5 <- reactive({
    as.numeric(input$bracketV5)
  })
  bracketVal6 <- reactive({
    as.numeric(input$bracketV6)
  })
  bracketVal7 <- reactive({
    as.numeric(input$bracketV7)
  })
  bracketVal8 <- reactive({
    as.numeric(input$bracketV8)
  })
  
  ## these aren't showing up as numeric
  bracket1T <- reactive({
    as.numeric(input$bracket1T)
    #print(input$bracket1) ## make sure doesn't have % included
    
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

  

  dataInput <- reactive({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)

    if (input$extraBracket1) {
      taxRate <- c(taxRate, input$bracket5)
    }
    if (input$extraBracket2) {
      taxRate <- c(taxRate, input$bracket6)
    }
    if (input$extraBracket3) {
      taxRate <- c(taxRate, input$bracket7)
    }
    if (input$extraBracket4) {
      taxRate <- c(taxRate, input$bracket8)
    }

    xval <- 10^seq(log10(10e6), log10(45e9), by = 0.001) ## get uniform on log scale


    idx1 <- xval <= bracketVal2() * 1e6
    idx2 <- xval > bracketVal2() * 1e6 & xval <= bracketVal3() * 1e6
    idx3 <- xval > bracketVal3() * 1e6 & xval <= bracketVal4() * 1e6

    if (input$extraBracket4) { ## since nested, test this one first
      idx4 <- xval > bracketVal4() * 1e6 & xval <= input$bracketV5 * 1e6
      idx5 <- xval > input$bracketV5 * 1e6 & xval <= input$bracketV6 * 1e6
      idx6 <- xval > input$bracketV6 * 1e6 & xval <= input$bracketV7 * 1e6
      idx7 <- xval > input$bracketV7 * 1e6 & xval <= input$bracketV8 * 1e6
      idx8 <- xval > input$bracketV8
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8)
    } else if (input$extraBracket3) {
      idx4 <- xval > bracketVal4() * 1e6 & xval <= input$bracketV5 * 1e6
      idx5 <- xval > input$bracketV5 * 1e6 & xval <= input$bracketV6 * 1e6
      idx6 <- xval > input$bracketV6 * 1e6 & xval <= input$bracketV7 * 1e6
      idx7 <- xval > input$bracketV7 * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7)
    } else if (input$extraBracket2) {
      idx4 <- xval > bracketVal4() * 1e6 & xval <= input$bracketV5 * 1e6
      idx5 <- xval > input$bracketV5 * 1e6 & xval <= input$bracketV6 * 1e6
      idx6 <- xval > input$bracketV6 * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6)
    } else if (input$extraBracket1) {
      idx4 <- xval > bracketVal4() * 1e6 & xval <= input$bracketV5 * 1e6
      idx5 <- xval > input$bracketV5 * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5)
    } else {
      idx4 <- xval > bracketVal4() * 1e6
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

    brackets <- c(bracketVal1(), bracketVal2(), bracketVal3(), bracketVal4())
    if (input$extraBracket1) {
      brackets <- c(brackets, input$bracketV5)
    }
    if (input$extraBracket2) {
      brackets <- c(brackets, input$bracketV6)
    }
    if (input$extraBracket3) {
      brackets <- c(brackets, input$bracketV7)
    }
    if (input$extraBracket4) {
      brackets <- c(brackets, input$bracketV8)
    }


    toPlot2$marginalInt <- unlist(lapply(toPlot2$xval, getAverageTax, taxRate, brackets))

    toPlot2$marginalRate <- (toPlot2$marginalInt / toPlot2$xval) * 100


    toPlot2$id <- 1:nrow(toPlot2)

    toPlot2
  })
  
  dataInputT <- reactive({
   #browser()
    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    
    if (input$extraBracket1) {
      taxRate <- c(taxRate, input$bracket5T)
    }
    if (input$extraBracket2) {
      taxRate <- c(taxRate, input$bracket6T)
    }
    if (input$extraBracket3) {
      taxRate <- c(taxRate, input$bracket7T)
    }
    if (input$extraBracket4) {
      taxRate <- c(taxRate, input$bracket8T)
    }
    
    xval <- 10^seq(log10(10e6), log10(45e9), by = 0.001) ## get uniform on log scale
    
    
    idx1 <- xval <= bracketVal2T() * 1e6
    idx2 <- xval > bracketVal2T() * 1e6 & xval <= bracketVal3T() * 1e6
    idx3 <- xval > bracketVal3T() * 1e6 & xval <= bracketVal4T() * 1e6
    
    if (input$extraBracket4) { ## since nested, test this one first
      idx4 <- xval > bracketVal4T() * 1e6 & xval <= input$bracketV5T * 1e6
      idx5 <- xval > input$bracketV5T * 1e6 & xval <= input$bracketV6T * 1e6
      idx6 <- xval > input$bracketV6T * 1e6 & xval <= input$bracketV7T * 1e6
      idx7 <- xval > input$bracketV7T * 1e6 & xval <= input$bracketV8T * 1e6
      idx8 <- xval > input$bracketV8T
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8)
    } else if (input$extraBracket3) {
      idx4 <- xval > bracketVal4T() * 1e6 & xval <= input$bracketV5T * 1e6
      idx5 <- xval > input$bracketV5T * 1e6 & xval <= input$bracketV6T * 1e6
      idx6 <- xval > input$bracketV6T * 1e6 & xval <= input$bracketV7T * 1e6
      idx7 <- xval > input$bracketV7T * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6, idx7)
    } else if (input$extraBracket2) {
      idx4 <- xval > bracketVal4T() * 1e6 & xval <= input$bracketV5T * 1e6
      idx5 <- xval > input$bracketV5T * 1e6 & xval <= input$bracketV6T * 1e6
      idx6 <- xval > input$bracketV6T * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5, idx6)
    } else if (input$extraBracket1) {
      idx4 <- xval > bracketVal4T() * 1e6 & xval <= input$bracketV5T * 1e6
      idx5 <- xval > input$bracketV5T * 1e6
      idx <- cbind.data.frame(idx1, idx2, idx3, idx4, idx5)
    } else {
      idx4 <- xval > bracketVal4T() * 1e6
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
    
    brackets <- c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T())
    if (input$extraBracket1) {
      brackets <- c(brackets, input$bracketV5T)
    }
    if (input$extraBracket2) {
      brackets <- c(brackets, input$bracketV6T)
    }
    if (input$extraBracket3) {
      brackets <- c(brackets, input$bracketV7T)
    }
    if (input$extraBracket4) {
      brackets <- c(brackets, input$bracketV8T)
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
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
    if (input$extraBracket1) {
      taxRate <- c(taxRate, input$bracket5)
    }
    if (input$extraBracket2) {
      taxRate <- c(taxRate, input$bracket6)
    }
    if (input$extraBracket3) {
      taxRate <- c(taxRate, input$bracket7)
    }
    if (input$extraBracket4) {
      taxRate <- c(taxRate, input$bracket8)
    }
    taxRateP <- taxRate / 100 ## get to percentage
    bracketStarts <- 1e6 * c(input$bracketV1, input$bracketV2, input$bracketV3, input$bracketV4)
    if (input$extraBracket1) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV5)
    }
    if (input$extraBracket2) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV6)
    }
    if (input$extraBracket3) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV7)
    }
    if (input$extraBracket4) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV8)
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
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
    if (input$extraBracket1) {
      taxRate <- c(taxRate, input$bracket5)
    }
    if (input$extraBracket2) {
      taxRate <- c(taxRate, input$bracket6)
    }
    if (input$extraBracket3) {
      taxRate <- c(taxRate, input$bracket7)
    }
    if (input$extraBracket4) {
      taxRate <- c(taxRate, input$bracket8)
    }
    taxRateP <- taxRate / 100 ## get to percentage

    bracketStarts <- 1e6 * c(input$bracketV1, input$bracketV2, input$bracketV3, input$bracketV4)
    if (input$extraBracket1) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV5)
    }
    if (input$extraBracket2) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV6)
    }
    if (input$extraBracket3) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV7)
    }
    if (input$extraBracket4) {
      bracketStarts <- c(bracketStarts, 1e6 * input$bracketV8)
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
    
    if(input$interface==1){
      taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4)
      if (input$extraBracket1) {
        taxRate <- c(taxRate, input$bracket5)
      }
      if (input$extraBracket2) {
        taxRate <- c(taxRate, input$bracket6)
      }
      if (input$extraBracket3) {
        taxRate <- c(taxRate, input$bracket7)
      }
      if (input$extraBracket4) {
        taxRate <- c(taxRate, input$bracket8)
      }
      
      # These are mini data set that ggvis needs to create vertical lines
      extra0 <- cbind.data.frame(x = rep(bracketVal1() * 1e6, 2), y = c(0, taxRate[1]))
      extra1 <- cbind.data.frame(x = rep(bracketVal2() * 1e6, 2), y = c(0, taxRate[1]))
      extra1b <- cbind.data.frame(x = rep(bracketVal2() * 1e6, 2), y = c(0, taxRate[2]))
      extra2 <- cbind.data.frame(x = rep(bracketVal3() * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(bracketVal3() * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(bracketVal4() * 1e6, 2), y = c(0, taxRate[4]))
      if (input$extraBracket1) {
        extra4 <- cbind.data.frame(x = rep(input$bracketV5 * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(input$bracketV5 * 1e6, 2), y = c(0, taxRate[5]))
      }
      if (input$extraBracket2) {
        extra5 <- cbind.data.frame(x = rep(input$bracketV6 * 1e6, 2), y = c(0, taxRate[5]))
        extra5b <- cbind.data.frame(x = rep(input$bracketV6 * 1e6, 2), y = c(0, taxRate[6]))
      }
      if (input$extraBracket3) {
        extra6 <- cbind.data.frame(x = rep(input$bracketV7 * 1e6, 2), y = c(0, taxRate[6]))
        extra6b <- cbind.data.frame(x = rep(input$bracketV7 * 1e6, 2), y = c(0, taxRate[7]))
      }
      if (input$extraBracket4) {
        extra7 <- cbind.data.frame(x = rep(input$bracket8 * 1e6, 2), y = c(0, taxRate[7]))
        extra7b <- cbind.data.frame(x = rep(input$bracketV8 * 1e6, 2), y = c(0, taxRate[8]))
      }
      
    }else{
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
      if (input$extraBracket1) {
        taxRate <- c(taxRate, input$bracket5T)
      }
      if (input$extraBracket2) {
        taxRate <- c(taxRate, input$bracket6T)
      }
      if (input$extraBracket3) {
        taxRate <- c(taxRate, input$bracket7T)
      }
      if (input$extraBracket4) {
        taxRate <- c(taxRate, input$bracket8T)
      }
      
      # These are mini data set that ggvis needs to create vertical lines
      extra0 <- cbind.data.frame(x = rep(bracketVal1T() * 1e6, 2), y = c(0, taxRate[1]))
      extra1 <- cbind.data.frame(x = rep(bracketVal2T() * 1e6, 2), y = c(0, taxRate[1]))
      extra1b <- cbind.data.frame(x = rep(bracketVal2T() * 1e6, 2), y = c(0, taxRate[2]))
      extra2 <- cbind.data.frame(x = rep(bracketVal3T() * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(bracketVal3T() * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(bracketVal4T() * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(bracketVal4T() * 1e6, 2), y = c(0, taxRate[4]))
      if (input$extraBracket1) {
        extra4 <- cbind.data.frame(x = rep(input$bracketV5T * 1e6, 2), y = c(0, taxRate[4]))
        extra4b <- cbind.data.frame(x = rep(input$bracketV5T * 1e6, 2), y = c(0, taxRate[5]))
      }
      if (input$extraBracket2) {
        extra5 <- cbind.data.frame(x = rep(input$bracketV6T * 1e6, 2), y = c(0, taxRate[5]))
        extra5b <- cbind.data.frame(x = rep(input$bracketV6T * 1e6, 2), y = c(0, taxRate[6]))
      }
      if (input$extraBracket3) {
        extra6 <- cbind.data.frame(x = rep(input$bracketV7T * 1e6, 2), y = c(0, taxRate[6]))
        extra6b <- cbind.data.frame(x = rep(input$bracketV7T * 1e6, 2), y = c(0, taxRate[7]))
      }
      if (input$extraBracket4) {
        extra7 <- cbind.data.frame(x = rep(input$bracket8T * 1e6, 2), y = c(0, taxRate[7]))
        extra7b <- cbind.data.frame(x = rep(input$bracketV8T * 1e6, 2), y = c(0, taxRate[8]))
      }
      
    }
    
    ## rename to showAvg

    showAvg <- function(x) {
      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      # https://stackoverflow.com/questions/31230124/exclude-line-points-from-showing-info-when-using-add-tooltip-with-hover-in-ggvis
      if (sum(grepl("id", names(x))) == 0) return(NULL)
      if (is.null(x)) return(NULL)
      
      if(input$interface==1){
        data <- dataInput()
      }else{
        data <- dataInputT()
        
      }
    

      row <- data[data$id == x$id, ]

      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", " <br> Wealth ($m): ", round(row$xval / 1e6, 0), "<br> Taxes Paid ($m) ", round(row$marginalInt / 1e6, 2), sep = "") ## dividing by 1e6 may need to change if we do this for xval overall
    }

   # plot <- dataInput()[, -ncol(dataInput())] %>%

    if(input$interface==1){
      data <- dataInput()
    }else{
      data <- dataInputT()
      
    }
    
    #browser()
    rmIdx=ncol(data)
      plot <- data[,-rmIdx] %>%
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
    if (input$extraBracket4) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7b, ~ x / 1e6, ~y)
    } else if (input$extraBracket3) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y)
    } else if (input$extraBracket2) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y)
    } else if (input$extraBracket1) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y)
    } else {
      plot
    }
  })


  vis2 %>% bind_shiny("plot2")
}