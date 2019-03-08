
server <- function(input, output, session) {
  # https://stackoverflow.com/questions/40670288/show-hide-inputs-based-on-numericinput-and-actionbutton
  observeEvent(input$submit, {
    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      req(input$bracket5T)
      taxRate <- c(taxRate, as.numeric(input$bracket5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      req(input$bracket6T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      req(input$bracket7T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      req(input$bracket8T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T), as.numeric(input$bracket8T))
    }

    brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      req(input$bracketV5T)
      brackets <- c(brackets, as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      req(input$bracketV6T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      req(input$bracketV7T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      req(input$bracketV8T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T), as.numeric(input$bracketV8T))
    }


    reorderIdx <- order(as.numeric(brackets))
    brackets <- brackets[reorderIdx]
    taxRate <- taxRate[reorderIdx]



    updateTextInput(session, "bracketV1T", label = paste("to the top ", getPercentile(updateGrid(), brackets[1]), "%'s wealth above ($m):", sep = ""), value = brackets[1])

    updateTextInput(session, "bracketV2T", label = paste("to the top ", getPercentile(updateGrid(), brackets[2]), "%'s wealth above ($m):", sep = ""), value = brackets[2])

    updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), brackets[3]), "%'s wealth above ($m):", sep = ""), value = brackets[3])

    updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), brackets[4]), "%'s wealth above ($m):", sep = ""), value = brackets[4])

    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), brackets[5]), "%'s wealth above ($m):", sep = ""), value = brackets[5])
      }
    }

    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), brackets[6]), "%'s wealth above ($m):", sep = ""), value = brackets[6])
      }
    }

    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), brackets[7]), "%'s wealth above ($m):", sep = ""), value = brackets[7])
      }
    }

    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal8T()), "%'s wealth above ($m):", sep = ""), value = bracketVal8T())
      }
    }

    updateTextInput(session, "bracket1T", value = taxRate[1])
    updateTextInput(session, "bracket2T", value = taxRate[2])
    updateTextInput(session, "bracket3T", value = taxRate[3])
    updateTextInput(session, "bracket4T", value = taxRate[4])

    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracket5T)) {
        updateTextInput(session, "bracket5T", value = taxRate[5])
      }
    }

    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracket6T)) {
        updateTextInput(session, "bracket6T", value = taxRate[6])
      }
    }

    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracket7T)) {
        updateTextInput(session, "bracket7T", value = taxRate[7])
      }
    }

    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracket8T)) {
        updateTextInput(session, "bracket8T", value = taxRate[8])
      }
    }
  })

  ## reset everything when you click reset
  observeEvent(input$reset, {
    reset("extraBrackets")
    reset("evasion")
    reset("bracket1T")
    reset("bracket2T")
    reset("bracket3T")
    reset("bracket4T")


    reset("bracketV1T")
    reset("bracketV2T")
    reset("bracketV3T")
    reset("bracketV4T")

    if (!is.null(input$bracketV5T)) {
      reset("bracketV5T")
      reset("bracket5T")
    }

    if (!is.null(input$bracketV6T)) {
      reset("bracketV6T")
      reset("bracket6T")
    }
    if (!is.null(input$bracketV7T)) {
      reset("bracketV7T")
      reset("bracket7T")
    }

    if (!is.null(input$bracketV8T)) {
      reset("bracketV8T")
      reset("bracket8T")
    }

    # click("submit") ## Not working
  })

  ## https://stackoverflow.com/questions/39627760/conditional-panel-in-shiny-doesnt-update-variables

  ## add brackets - tax rate
  output$myui <- renderUI({
    req(input$extraBrackets)
    if (input$extraBrackets == 3) {
      shinyjs::hide(id = "bracket4T")
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket2T")
    }

    if (input$extraBrackets == 2) {
      shinyjs::hide(id = "bracket3T")
      shinyjs::hide(id = "bracket4T")
    }

    if (input$extraBrackets == 4) {
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket4T")
    }

    if (input$extraBrackets == 5) {
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket4T")
      textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
    } else if (input$extraBrackets == 6) {
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket4T")
      tagList(
        textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),

        textInput("bracket6T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
      )
    } else if (input$extraBrackets == 7) {
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket4T")
      tagList(
        textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),

        textInput("bracket6T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),
        textInput("bracket7T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
      )
    } else if (input$extraBrackets == 8) {
      shinyjs::show(id = "bracket3T")
      shinyjs::show(id = "bracket4T")
      tagList(
        textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),

        textInput("bracket6T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),
        textInput("bracket7T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),
        textInput("bracket8T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
      )
    }
  })

  ## add brackets - value
  output$myui2 <- renderUI({
    req(input$extraBrackets)
    if (input$extraBrackets == 3) {
      shinyjs::hide(id = "bracketV4T")
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV2T")
    }

    if (input$extraBrackets == 2) {
      shinyjs::hide(id = "bracketV3T")
      shinyjs::hide(id = "bracketV4T")
    }

    if (input$extraBrackets == 4) {
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV4T")
    }
    if (input$extraBrackets == 5) {
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV4T")
      textInput("bracketV5T", label = "to wealth above ($m):", value = "1500")
    } else if (input$extraBrackets == 6) {
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV4T")
      tagList(
        textInput("bracketV5T", label = "to wealth above ($m):", value = "1500"),


        textInput("bracketV6T", label = "to wealth above ($m):", value = "1600")
      )
    } else if (input$extraBrackets == 7) {
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV4T")
      tagList(
        textInput("bracketV5T", label = "to wealth above ($m):", value = "1500"),


        textInput("bracketV6T", label = "to wealth above ($m):", value = "1600"),
        textInput("bracketV7T", label = "to wealth above ($m):", value = "1700")
      )
    } else if (input$extraBrackets == 8) {
      shinyjs::show(id = "bracketV3T")
      shinyjs::show(id = "bracketV4T")
      tagList(
        textInput("bracketV5T", label = "to wealth above ($m):", value = "1500"),


        textInput("bracketV6T", label = "to wealth above ($m):", value = "1600"),
        textInput("bracketV7T", label = "to wealth above ($m):", value = "1700"),
        textInput("bracketV8T", label = "to wealth above ($m):", value = "1900")
      )
    }
  })

  grid <- read.csv("taxBaseGridUpdated.csv")

  ## update data with evasion parameter
  updateGrid <- reactive({
    req(input$evasion)
    if(input$evasion=="."){
     evas=0 ## if accidentally left here, will become zero
    }else{
      evas=input$evasion
    }
    grid$thresNew <- (1 - as.numeric(evas) / 100) * grid$thres
    grid$avgNew <- (1 - as.numeric(evas) / 100) * grid$avg ##
    # browser()
    grid <- rbind(grid, c(100, 0, NA, NA, NA, grid$thresNew[nrow(grid)] + 1, 0)) ## fake extra row with nobody in it
    return(grid)
    
  })

  ## get percentile for display
  ## after evasion
  getPercentile <- function(grid, value) {
    perc <- grid$gperc[which.min(abs(grid$thresNew - value * 1e6))]
    return(format(round(100 - perc, 5), scientific = F))
  }

  getNextPercentile <- function(grid, value) {
    perc <- which.min(abs(grid$thresNew - value * 1e6))

    if (grid$thresNew[perc] < value) {
      perc <- perc + 1
    }
    perc <- perc + 1 ## bump one up
    return(grid$thresNew[perc] / 1e6)
  }


  getPercentileMarkers <- function(grid) {
    c(
      grid$thresNew[which(grid$gperc == 90)],
      grid$thresNew[which(grid$gperc == 99)],
      grid$thresNew[which(grid$gperc >= 99.9 & grid$gperc < 99.99)[1]],
      grid$thresNew[which(grid$gperc >= 99.99 & grid$gperc < 99.999)[1]]
    )
  }
  ## gets taxes paid per bracket
  getTaxBasePerBracket <- function(grid, taxLevels, brackets) {
    first_nonzero_tax_brack <- brackets[min(which(taxLevels>0))]
    grid_aux <- grid %>% filter(thresNew > first_nonzero_tax_brack)
    test <- unlist(lapply(grid_aux$avgNew, getAverageTax, taxLevels, brackets / 1e6))

    return(sum(grid_aux$nb * test))
  }

  ## gets people per bracket
  getPeoplePerBracket <- function(grid, brackets) {
    brackets <- c(brackets, max(grid$thresNew) + 1e6) ## get last bracket
    grid$group <- cut(grid$thresNew, brackets, include.lowest = T)
    toReturn <- grid %>% group_by(group) %>% summarise(totalPeople = sum(nb)) %>% drop_na() %>% complete(group, fill = list(totalPeople = 0)) ## avoid dropping levels without any people

    # https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in

    return(toReturn)
  }


  # https://github.com/rstudio/shiny/issues/1140
  ## update percentile for new bracket value
  observe({
    updateTextInput(session, "bracketV1T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T()), "%'s wealth above ($m):", sep = ""))
  })

  observe({
    updateTextInput(session, "bracketV2T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T()), "%'s wealth above ($m):", sep = ""))
  })

  observe({
    updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T()), "%'s wealth above ($m):", sep = ""))
  })

  observe({
    updateTextInput(session, "bracketV4T", paste("to the top ", getPercentile(updateGrid(), bracketVal4T()), "%'s wealth above ($m):", sep = ""))
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
       
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal5T()), "%'s wealth above ($m):", sep = ""), value = bracketVal5T())
        
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal6T()), "%'s wealth above ($m):", sep = ""), value = bracketVal6T())
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal7T()), "%'s wealth above ($m):", sep = ""), value = bracketVal7T())
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal8T()), "%'s wealth above ($m):", sep = ""), value = bracketVal8T())
      }
    }
  })

  ## update if make two brackets the same
  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal1T() == bracketVal2T()) {
      # browser()

      updateTextInput(session, "bracketV2T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal1T() == bracketVal3T()) {
      updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal1T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal2T() == bracketVal3T()) {
      updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal2T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    if (bracketVal3T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal3T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal3T()), 2))
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal4T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal4T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal4T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal3T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal3T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal3T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal2T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal1T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal5T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal5T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal5T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal4T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal4T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal4T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal3T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal3T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal3T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal2T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal1T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal6T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal6T()), 2)), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal6T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal5T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal5T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal5T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal4T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal4T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal4T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal3T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal3T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal3T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal2T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal1T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal7T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal7T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal7T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal6T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal6T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal6T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal5T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal5T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal5T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal4T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal4T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal4T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal3T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal3T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal3T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal2T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal2T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal2T()), 2))
      }
    }
  })

  observeEvent(input$submit, ignoreNULL = FALSE, {
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal1T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), getPercentile(updateGrid(), round(getNextPercentile(updateGrid(), bracketVal1T()), 2))), "%'s wealth above ($m):", sep = ""), value = round(getNextPercentile(updateGrid(), bracketVal1T()), 2))
      }
    }
  })







  ## boundaries for evasion parameter
  observe({
    if (input$evasion != "" & input$evasion!=".") {
      if (as.numeric(input$evasion) < 0) {
        updateTextInput(session, "evasion", value = "0")
      }
    }
  })

  observe({
    if (input$evasion != "" & input$evasion!=".") {
      if (as.numeric(input$evasion) > 99) {
        updateTextInput(session, "evasion", value = "99")
      }
    }
  })

  ## don't let negative tax rates
  observe({
    if (!is.na(bracket1T()) & bracket1T() < 0) {
      updateTextInput(session, "bracket1T", value = 0)
    }
  })

  observe({
    if (!is.na(bracket2T()) & bracket2T() < 0) {
      updateTextInput(session, "bracket2T", value = 0)
    }
  })
  observe({
    if (!is.na(bracket3T()) & bracket3T() < 0) {
      updateTextInput(session, "bracket3T", value = 0)
    }
  })

  observe({
    if (!is.na(bracket4T()) & bracket4T() < 0) {
      updateTextInput(session, "bracket4T", value = 0)
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracket5T)) {
        if (!is.na(bracket5T()) & bracket5T() < 0) {
          updateTextInput(session, "bracket5T", value = 0)
        }
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracket6T)) {
        if (!is.na(bracket6T()) & bracket6T() < 0) {
          updateTextInput(session, "bracket6T", value = 0)
        }
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracket7T)) {
        if (!is.na(bracket7T()) & bracket7T() < 0) {
          updateTextInput(session, "bracket7T", value = 0)
        }
      }
    }
  })

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracket8T)) {
        if (!is.na(bracket8T()) & bracket8T() < 0) {
          updateTextInput(session, "bracket8T", value = 0)
        }
      }
    }
  })

  ## don't let  tax brackets go below 0 or above max wealth after evasion

  observe({
    if (bracketVal1T() < 0) {
      updateTextInput(session, "bracketV1T", value = 0)
    }
  })

  # observe({
  #   if (bracketVal1T() > max(updateGrid()$thresNew) / 1e6) {
  #     updateTextInput(session, "bracketV1T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #     print(max(updateGrid()$thresNew))
  #   }
  # })


  observe({
    if (bracketVal2T() < 0) {
      updateTextInput(session, "bracketV2T", value = 0)
    }
  })
  #
  #     observe({
  #       if (bracketVal2T() > max(updateGrid()$thresNew) / 1e6) {
  #         updateTextInput(session, "bracketV2T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #         print(max(updateGrid()$thresNew))
  #       }
  #     })
  #   })


  observe({
    if (bracketVal3T() < 0) {
      updateTextInput(session, "bracketV3T", value = 0)
    }
  })

  # observe({
  #   if (bracketVal3T() > max(updateGrid()$thresNew) / 1e6) {
  #     updateTextInput(session, "bracketV3T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #     print(max(updateGrid()$thresNew))
  #   }
  # })


  observe({
    if (bracketVal4T() < 0) {
      updateTextInput(session, "bracketV4T", value = 0)
    }
  })

  # observe({
  #   if (bracketVal4T() > max(updateGrid()$thresNew) / 1e6) {
  #     updateTextInput(session, "bracketV4T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #     # print(max(updateGrid()$thresNew))
  #   }
  # })
  #

  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
        if (bracketVal5T() < 0) {
          updateTextInput(session, "bracketV5T", value = 0)
        }
      }
    }
  })

  # observe({
  #   if (input$extraBrackets >= 5) {
  #     if (!is.null(input$bracketV5T)) {
  #       if (bracketVal5T() > max(updateGrid()$thresNew) / 1e6) {
  #         updateTextInput(session, "bracketV5T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #       }
  #     }
  #   }
  # })


  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        if (bracketVal6T() < 0) {
          updateTextInput(session, "bracketV6T", value = 0)
        }
      }
    }
  })

  # observe({
  #   if (input$extraBrackets >= 6) {
  #     if (!is.null(input$bracketV6T)) {
  #       if (bracketVal6T() > max(updateGrid()$thresNew) / 1e6) {
  #         updateTextInput(session, "bracketV6T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #       }
  #     }
  #   }
  # })


  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        if (bracketVal7T() < 0) {
          updateTextInput(session, "bracketV7T", value = 0)
        }
      }
    }
  })

  # observe({
  #   if (input$extraBrackets >= 7) {
  #     if (!is.null(input$bracketV7T)) {
  #       if (bracketVal7T() > max(updateGrid()$thresNew) / 1e6) {
  #         updateTextInput(session, "bracketV7T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #       }
  #     }
  #   }
  # })


  observe({
    req(input$extraBrackets)
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        if (bracketVal8T() < 0) {
          updateTextInput(session, "bracketV8T", value = 0)
        }
      }
    }
  })

  # observe({
  #   if (input$extraBrackets >= 8) {
  #     if (!is.null(input$bracketV8T)) {
  #       if (bracketVal8T() > max(updateGrid()$thresNew) / 1e6) {
  #         updateTextInput(session, "bracketV8T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
  #       }
  #     }
  #   }
  # })


  ## helpers to call these values
  bracket1T <- reactive({
    req(input$bracket1T)
    as.numeric(input$bracket1T)
  })
  bracket2T <- reactive({
    req(input$bracket2T)
    as.numeric(input$bracket2T)
  })
  bracket3T <- reactive({
    req(input$bracket3T)
    as.numeric(input$bracket3T)
  })
  bracket4T <- reactive({
    req(input$bracket4T)
    as.numeric(input$bracket4T)
  })
  bracket5T <- reactive({
    if (input$extraBrackets >= 5) {
      req(input$bracket5T)
    }
    as.numeric(input$bracket5T)
  })
  bracket6T <- reactive({
    if (input$extraBrackets >= 6) {
      req(input$bracket6T)
    }
    as.numeric(input$bracket6T)
  })
  bracket7T <- reactive({
    if (input$extraBrackets >= 7) {
      req(input$bracket7T)
    }
    as.numeric(input$bracket7T)
  })
  bracket8T <- reactive({
    if (input$extraBrackets >= 8) {
      req(input$bracket8T)
    }
    as.numeric(input$bracket8T)
  })

  bracketVal1T <- reactive({
    req(input$bracketV1T)
    if(input$bracketV1T=="."){
      return(0)
    }else{
    return(as.numeric(input$bracketV1T))
    }
  })
  bracketVal2T <- reactive({
    req(input$bracketV2T)
    if(input$bracketV2T=="."){
      return(0)
    }else{
    return(as.numeric(input$bracketV2T))
    }
  })
  bracketVal3T <- reactive({
    req(input$bracketV3T)
    if(input$bracketV3T=="."){
      return(0)
    }else{
      return(as.numeric(input$bracketV3T))
      
    }
  })
  bracketVal4T <- reactive({
    req(input$bracketV4T)
    if(input$bracketV4T=="."){
      return(0)
    }else{
      return(as.numeric(input$bracketV4T))
    }
  })
  bracketVal5T <- reactive({
    if (input$extraBrackets >= 5) {
      req(input$bracketV5T)
    }
    if(input$bracketV5T=="."){
      return(0)
    }else{
    return(as.numeric(input$bracketV5T))
    }
  })
  bracketVal6T <- reactive({
    if (input$extraBrackets >= 6) {
      req(input$bracketV6T)
    }
    if(input$bracketV6T=="."){
      return(0)
    }else{
      return(as.numeric(input$bracketV6T))
      
    }
  })
  bracketVal7T <- reactive({
    if (input$extraBrackets >= 7) {
      req(input$bracketV7T)
    }
    if(input$bracketV7T=="."){
      return(0)
    }else{
      return(as.numeric(input$bracketV7T))
    }
  })
  bracketVal8T <- reactive({
    if (input$extraBrackets >= 8) {
      req(input$bracketV8T)
    }
    if(input$bracketV8T=="."){
      return(0)
    }else{
      return(as.numeric(input$bracketV8T))
    }
  })




  ## get data ready to plot
  dataInputT <- reactive({
    if (input$extraBrackets >= 2) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T))
    }

    if (input$extraBrackets >= 3) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T))
    }



    if (input$extraBrackets >= 4) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T), as.numeric(input$bracket8T))
    }

    # taxRate <- c(taxRate, 0) ## fake large bracket

    if (input$extraBrackets >= 2) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T()))
    }

    if (input$extraBrackets >= 3) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }
    if (input$extraBrackets >= 4) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }




    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T), as.numeric(input$bracketV8T))
    }

    # brackets <- c(brackets,1e100) ## fake large bracket

    ## reshuffle to make sure brackets are increasing
    ## tax rates not forced to be monotonic
    reorderIdx <- order(as.numeric(brackets))
    brackets <- brackets[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    xval <- 10^seq(log10(1e5), log10(max(updateGrid()$thresNew)), by = 0.001) ## get uniform on log scale

    if (brackets[length(brackets)] > max(updateGrid()$thresNew) / 1e6) {
      xval <- 10^seq(log10(1e5), log10(brackets[length(brackets)] * 1e6), by = 0.001) ## get uniform on log scale
    }

    if (input$extraBrackets == 2) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2)
    } else if (input$extraBrackets == 3) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3)
    } else if (input$extraBrackets == 4) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6
      idx4 <- xval > as.numeric(brackets[4])
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4)
    } else if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6

      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6 & xval <= as.numeric(brackets[7]) * 1e6
      idx7 <- xval > as.numeric(brackets[7]) * 1e6 & xval <= as.numeric(brackets[8]) * 1e6
      idx8 <- xval > as.numeric(brackets[8])
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8)
    } else if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6 & xval <= as.numeric(brackets[7]) * 1e6
      idx7 <- xval > as.numeric(brackets[7]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7)
    } else if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6)
    } else if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      idx0 <- xval <= as.numeric(brackets[1]) * 1e6
      idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
      idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
      idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5)
    }


    # Indicator across income on tax bracket position
    getGroup <- unlist(apply(idx, 1, function(x) {
      which(x)[1]
    }))


    toPlot <- cbind.data.frame(xval, getGroup)

    ## add an extra ghost bracket to the end that isn't taxed
    toMatch <- cbind.data.frame(group = 1:(length(taxRate) + 1), tax = c(0, taxRate))

    toPlot2 <- merge(toPlot, toMatch, by.x = "getGroup", by.y = "group")


    # unaffected by new grouping
    toPlot2$marginalInt <- unlist(lapply(toPlot2$xval, getAverageTax, taxRate, brackets))

    toPlot2$marginalRate <- (toPlot2$marginalInt / toPlot2$xval) * 100


    head(toPlot2)
    toPlot2$id <- 1:nrow(toPlot2)

    toPlot2
  })


  # Computes total tax revenue, avg happens later
  getAverageTax <- function(wealth, taxLevels, brackets) {
    ## pass in brackets to make sure they update
    ## expecting taxLevels in percentage

    taxLevels <- taxLevels / 100
    first <- wealth - brackets[1] * 1e6
    second <- first - (brackets[2] * 1e6 - brackets[1] * 1e6)

    if (length(taxLevels) == 3) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
    }
    if (length(taxLevels) == 4) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
      fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
    }
    if (length(taxLevels) == 5) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
      fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)
    }
    if (length(taxLevels) == 6) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
      fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
    }
    if (length(taxLevels) == 7) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
      fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
      seventh <- sixth - (brackets[7] * 1e6 - brackets[6] * 1e6)
    }
    if (length(taxLevels) == 8) {
      third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
      fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
      fifth <- fourth - (brackets[5] * 1e6 - brackets[4] * 1e6)

      sixth <- fifth - (brackets[6] * 1e6 - brackets[5] * 1e6)
      seventh <- sixth - (brackets[7] * 1e6 - brackets[6] * 1e6)
      eighth <- seventh - (brackets[8] * 1e6 - brackets[7] * 1e6)
    }

    firstChunk <- ifelse(second >= 0, taxLevels[1] * (brackets[2] * 1e6 - brackets[1] * 1e6), taxLevels[1] * max(first, 0))
    if (length(taxLevels) == 2) {
      secondChunk <- ifelse(second >= 0, second * taxLevels[2], 0)
      toReturn <- firstChunk + secondChunk
    }



    if (length(taxLevels) == 3) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(third >= 0, third * taxLevels[3], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk
    }
    if (length(taxLevels) == 4) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
      fourthChunk <- ifelse(fourth >= 0, fourth * taxLevels[4], 0)

      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk
    }


    if (length(taxLevels) == 8) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(seventh >= 0, taxLevels[6] * (brackets[7] * 1e6 - brackets[6] * 1e6), taxLevels[6] * max(sixth, 0))
      seventhChunk <- ifelse(eighth >= 0, taxLevels[7] * (brackets[8] * 1e6 - brackets[7] * 1e6), taxLevels[7] * max(seventh, 0))
      eighthChunk <- ifelse(eighth >= 0, eighth * taxLevels[8], 0)

      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk + seventhChunk + eighthChunk
    } else if (length(taxLevels) == 7) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(seventh >= 0, taxLevels[6] * (brackets[7] * 1e6 - brackets[6] * 1e6), taxLevels[6] * max(sixth, 0))
      seventhChunk <- ifelse(seventh >= 0, seventh * taxLevels[7], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk + seventhChunk
    } else if (length(taxLevels) == 6) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(sixth >= 0, taxLevels[5] * (brackets[6] * 1e6 - brackets[5] * 1e6), taxLevels[5] * max(fifth, 0))
      sixthChunk <- ifelse(sixth >= 0, sixth * taxLevels[6], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk
    } else if (length(taxLevels) == 5) {
      secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
      thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
      fourthChunk <- ifelse(fifth >= 0, taxLevels[4] * (brackets[5] * 1e6 - brackets[4] * 1e6), taxLevels[4] * max(fourth, 0))
      fifthChunk <- ifelse(fifth >= 0, fifth * taxLevels[5], 0)
      toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk
    }
    return(toReturn)
  }

  ## total tax helper
  totalTax <- eventReactive(input$submit, ignoreNULL = FALSE, {
    ## wait for brackets to be ready
    req(input$bracket1T)
    req(input$bracket2T)
    # req(input$bracket3T)
    # req(input$bracket4T)
    req(input$bracketV1T)
    req(input$bracketV2T)
    # req(input$bracketV3T)
    # req(input$bracketV4T)
    if (input$extraBrackets == 2) {
      taxRate <- as.numeric(c(bracket1T(), bracket2T()))
    }
    if (input$extraBrackets == 3) {
      taxRate <- as.numeric(c(bracket1T(), bracket2T(), bracket3T()))
    }

    if (input$extraBrackets >= 4) {
      taxRate <- as.numeric(c(bracket1T(), bracket2T(), bracket3T(), bracket4T()))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, bracket5T())
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, bracket5T(), bracket6T())
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, bracket5T(), bracket6T(), bracket7T())
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, bracket5T(), bracket6T(), bracket7T(), bracket8T())
    }
    taxRateP <- as.numeric(taxRate) / 100 ## get to percentage
    if (input$extraBrackets == 2) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T()))
    }

    if (input$extraBrackets == 3) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }

    if (input$extraBrackets >= 4) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()), 1e6 * as.numeric(bracketVal7T()))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()), 1e6 * as.numeric(bracketVal7T()), 1e6 * as.numeric(bracketVal8T()))
    }

    reorderIdx <- order(as.numeric(bracketStarts))
    bracketStarts <- bracketStarts[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    taxPerBracket <- getTaxBasePerBracket(updateGrid(), as.numeric(taxRate), as.numeric(bracketStarts))

    taxPerBracket / 1e9 ## in billions
  })

  ## total tax output
  output$totalTax <- renderText({
    round(totalTax(), 4)
  })

  ## total tax output over 10 years
  output$totalTax_10 <- renderText({
    totalTax10 <- totalTax() * 13

    round(totalTax10 / 1e3, 2)
  })

  ## total number of households taxed helper
  householdsTaxed <- eventReactive(input$submit, ignoreNULL = FALSE, {
    ## wait for things to be ready
    req(input$bracket1T)
    req(input$bracket2T)
    # req(input$bracket3T)
    # req(input$bracket4T)
    req(input$bracketV1T)
    req(input$bracketV2T)
    # req(input$bracketV3T)
    # req(input$bracketV4T)
    if (input$extraBrackets == 2) {
      taxRate <- as.numeric(c(bracketVal1T(), bracketVal2T()))
    }
    if (input$extraBrackets == 3) {
      taxRate <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }

    if (input$extraBrackets >= 4) {
      taxRate <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }



    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, bracketVal5T())
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, bracketVal5T(), bracketVal6T())
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, bracketVal5T(), bracketVal6T(), bracketVal7T())
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, bracketVal5T(), bracketVal6T(), bracketVal7T(), bracketVal8T())
    }
    taxRateP <- as.numeric(taxRate) / 100 ## get to percentage

    if (input$extraBrackets == 2) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T()))
    }
    if (input$extraBrackets == 3) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }


    if (input$extraBrackets >= 4) {
      bracketStarts <- 1e6 * as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()), 1e6 * as.numeric(bracketVal7T()))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(bracketVal5T()), 1e6 * as.numeric(bracketVal6T()), 1e6 * as.numeric(bracketVal7T()), 1e6 * as.numeric(bracketVal8T()))
    }

    reorderIdx <- order(as.numeric(bracketStarts))
    bracketStarts <- bracketStarts[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    peoplePerBracket <- getPeoplePerBracket(updateGrid(), bracketStarts)
    numberTaxpayers <- peoplePerBracket$totalPeople
    if (length(numberTaxpayers) != length(taxRateP)) {
      return(NA)
    } else {
      householdsTaxed <- numberTaxpayers * (taxRateP > 0)

      return(householdsTaxed)
    }
  })

  ## output total taxpayers
  output$totalTaxpayers <- renderText({
    totalTaxpayers <- sum(householdsTaxed())

    round(totalTaxpayers)
  })

  taxUnitsHelper <- eventReactive(input$submit, ignoreNULL = FALSE, {
    if (input$extraBrackets >= 2) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T))
    }
    if (input$extraBrackets >= 3) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T))
    }

    if (input$extraBrackets >= 4) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T), as.numeric(input$bracket8T))
    }

    if (input$extraBrackets >= 2) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T()))
    }
    if (input$extraBrackets >= 3) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }
    if (input$extraBrackets >= 4) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }


    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T), as.numeric(input$bracketV8T))
    }

    reorderIdx <- order(as.numeric(brackets))
    brackets <- brackets[reorderIdx]
    taxRate <- taxRate[reorderIdx]



    getPercentile(updateGrid(), brackets[which(taxRate > 0)[1]])
  })

  ## use percentile affected
  output$percentTaxUnits <- renderText({
    taxUnitsHelper()
  })


  # F F no
  # F T crash
  # T T crash
  # T F no
  vis2 <- eventReactive(input$submit, ignoreNULL = FALSE, {
    req(input$evasion)
    req(input$bracket1T)
    req(input$bracket2T)
    # req(input$bracket3T)
    # req(input$bracket4T)

    req(input$bracketV1T)
    req(input$bracketV2T)
    # req(input$bracketV3T)
    # req(input$bracketV4T)
    if (input$extraBrackets == 2) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T))
    }
    if (input$extraBrackets == 3) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T))
    }

    if (input$extraBrackets >= 4) {
      taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    }
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      req(input$bracket5T)
      taxRate <- c(taxRate, as.numeric(input$bracket5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      req(input$bracket6T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      req(input$bracket7T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      req(input$bracket8T)

      taxRate <- c(taxRate, as.numeric(input$bracket5T), as.numeric(input$bracket6T), as.numeric(input$bracket7T), as.numeric(input$bracket8T))
    }
    if (input$extraBrackets == 2) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T()))
    }

    if (input$extraBrackets >= 3) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T()))
    }
    if (input$extraBrackets >= 4) {
      brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
    }

    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      req(input$bracketV5T)
      brackets <- c(brackets, as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      req(input$bracketV6T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      req(input$bracketV7T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      req(input$bracketV8T)

      brackets <- c(brackets, as.numeric(input$bracketV5T), as.numeric(input$bracketV6T), as.numeric(input$bracketV7T), as.numeric(input$bracketV8T))
    }


    reorderIdx <- order(as.numeric(brackets))
    brackets <- brackets[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    # These are mini data set that ggvis needs to create vertical lines
    extra0 <- cbind.data.frame(x = rep(max(as.numeric(brackets[1]) * 1e6, 1e5), 2), y = c(0, taxRate[1]))
    extra1 <- cbind.data.frame(x = rep(as.numeric(brackets[2]) * 1e6, 2), y = c(0, taxRate[1]))
    extra1b <- cbind.data.frame(x = rep(as.numeric(brackets[2]) * 1e6, 2), y = c(0, taxRate[2]))
    if (input$extraBrackets == 3) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
    }
    if (input$extraBrackets >= 4) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
    }
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5 <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5b <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[6]))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5 <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5b <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6 <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6b <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[7]))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
      extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
      extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5 <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5b <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6 <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6b <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[7]))
      extra7 <- cbind.data.frame(x = rep(as.numeric(brackets[8]) * 1e6, 2), y = c(0, taxRate[7]))
      extra7b <- cbind.data.frame(x = rep(as.numeric(brackets[8]) * 1e6, 2), y = c(0, taxRate[8]))
    }


    showAvg <- function(x) {
      # https://stackoverflow.com/questions/28396900/r-ggvis-html-function-failing-to-add-tooltip/28399656#28399656
      # https://stackoverflow.com/questions/31230124/exclude-line-points-from-showing-info-when-using-add-tooltip-with-hover-in-ggvis
      if (sum(grepl("id", names(x))) == 0) return(NULL)
      if (is.null(x)) return(NULL)


      data <- dataInputT()

      # data = subset(data,data$xval<=45000)

      row <- data[data$id == x$id, ]

      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", " <br> Wealth ($m): ", round(row$xval / 1e6, 0), "<br> Top ", getPercentile(updateGrid(), row$xval / 1e6), "%", "<br> Taxes Paid ($m): ", round(row$marginalInt / 1e6, 2), sep = "") ## dividing by 1e6 may need to change if we do this for xval overall
    }



    data <- dataInputT()

    markers <- data.frame(a = c(getPercentileMarkers(updateGrid()) / 1e6, max(updateGrid()$thresNew) / 1e6), b = rep(0.25, 5), c = c("Top 10%", "Top 1%", "Top 0.1%", "Top 0.01%", "Maximum"))


    valuesInt <- c(brackets, round(max(updateGrid()$thresNew) / 1e6, 2), round(getPercentileMarkers(updateGrid()) / 1e6, 2))
    valuesInt <- sort(valuesInt)
    rmIdx <- ncol(data)
    plot <- data[, -rmIdx] %>%
      ggvis(x = ~ xval / 1e6, y = ~tax) %>%
      layer_points() %>%
      layer_points(data = subset(data, xval / 1e6 <= max(updateGrid()$thresNew) / 1e6), x = ~ xval / 1e6, y = ~marginalRate, stroke := "red", key := ~id) %>%
      add_tooltip(showAvg, "hover") %>%
      # layer_lines(data = subset(data,xval/1e6<=45000),x = ~ xval / 1e6, y = ~marginalRate, stroke := "red") %>%
      layer_paths(data = extra1, ~ x / 1e6, ~y) %>%
      # layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
      # layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra0, ~ x / 1e6, ~y) %>%
      layer_paths(data = extra1b, ~ x / 1e6, ~y) %>%
      # layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
      # layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
      layer_text(data = markers, ~a, ~b, text := ~c, align := "center", fontWeight := "bold") %>%
      add_axis("x",
        title_offset = 80, title = "Wealth ($m)", grid = F, format = ",",
        values = valuesInt, properties = axis_props(labels = list(angle = 45, align = "left", baseline = "middle"))
      ) %>%
      add_axis("y", title = "Tax rate (%)") %>%
      scale_numeric("x", trans = "log", expand = 0) %>%
      set_options(width = 1000, height = 500)


    if (input$extraBrackets == 3) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 4) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra7b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      plot %>%
        layer_paths(data = extra2, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra2b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra3b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y)
    } else {
      plot
    }
  })


  vis2 %>% bind_shiny("plot2")
}