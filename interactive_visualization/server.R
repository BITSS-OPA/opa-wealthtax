
server <- function(input, output, session) {

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


    click("submit") ## Not working
  })

  ## https://stackoverflow.com/questions/39627760/conditional-panel-in-shiny-doesnt-update-variables

  ## add brackets - tax rate
  output$myui <- renderUI({
    if (input$extraBrackets == 5) {
      textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
    } else if (input$extraBrackets == 6) {
      tagList(
        textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),

        textInput("bracket6T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
      )
    } else if (input$extraBrackets == 7) {
      tagList(
        textInput("bracket5T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),

        textInput("bracket6T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3"),
        textInput("bracket7T", label = HTML("Apply a tax of (%): <br/> <br/>"), value = "3")
      )
    } else if (input$extraBrackets == 8) {
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
    if (input$extraBrackets == 5) {
      textInput("bracketV5T", label = "to wealth above ($m):", value = "1500")
    } else if (input$extraBrackets == 6) {
      tagList(
        textInput("bracketV5T", label = "to wealth above ($m):", value = "1500"),


        textInput("bracketV6T", label = "to wealth above ($m):", value = "1600")
      )
    } else if (input$extraBrackets == 7) {
      tagList(
        textInput("bracketV5T", label = "to wealth above ($m):", value = "1500"),


        textInput("bracketV6T", label = "to wealth above ($m):", value = "1600"),
        textInput("bracketV7T", label = "to wealth above ($m):", value = "1700")
      )
    } else if (input$extraBrackets == 8) {
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
    grid$thresNew <- (1 - as.numeric(input$evasion) / 100) * grid$thres
    grid$avgNew <- (1 - as.numeric(input$evasion) / 100) * grid$avg ##
    return(grid)
  })

  ## get percentile for display
  ## after evasion
  getPercentile <- function(grid, value) {
    perc <- grid$gperc[which.min(abs(grid$thresNew - value * 1e6))]
    return(format(round(100 - perc, 5), scientific = F))
  }



  ## gets taxes paid per bracket
  getTaxBasePerBracket <- function(grid, taxLevels, brackets) {
    test <- unlist(lapply(grid$avgNew, getAverageTax, taxLevels, brackets / 1e6))

    return(sum(grid$nb * test))
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
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal5T()), "%'s wealth above ($m):", sep = ""), value = bracketVal5T())
      }
    }
  })

  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal6T()), "%'s wealth above ($m):", sep = ""), value = bracketVal6T())
      }
    }
  })

  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal7T()), "%'s wealth above ($m):", sep = ""), value = bracketVal7T())
      }
    }
  })

  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal8T()), "%'s wealth above ($m):", sep = ""), value = bracketVal8T())
      }
    }
  })

  ## update if make two brackets the same
  observe({
    if (bracketVal1T() == bracketVal2T()) {
      updateTextInput(session, "bracketV2T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
    }
  })
  
  observe({
    if (bracketVal1T() == bracketVal3T()) {
      updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
    }
  })
  
  observe({
    if (bracketVal1T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
    }
  })

  observe({
    if (bracketVal2T() == bracketVal3T()) {
      updateTextInput(session, "bracketV3T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
    }
  })
  
  observe({
    if (bracketVal2T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
    }
  })

  observe({
    if (bracketVal3T() == bracketVal4T()) {
      updateTextInput(session, "bracketV4T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal3T() + 10)
    }
  })

  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal4T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal4T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal4T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal3T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal3T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal2T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T) & bracketVal1T() == bracketVal5T()) {
        updateTextInput(session, "bracketV5T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
      }
    }
  })

  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal5T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal5T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal5T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal4T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal4T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal4T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal3T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal3T() + 10)
      }
    }
  })

  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal2T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T) & bracketVal1T() == bracketVal6T()) {
        updateTextInput(session, "bracketV6T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal6T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal6T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal6T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal5T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal5T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal5T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal4T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal4T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal4T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal3T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal3T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal2T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T) & bracketVal1T() == bracketVal7T()) {
        updateTextInput(session, "bracketV7T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
      }
    }
  })

  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal7T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal7T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal7T() + 10)
      }
    }
  })

  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal6T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal6T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal6T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal5T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal5T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal5T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal4T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal4T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal4T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal3T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal3T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal3T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal2T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal2T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal2T() + 10)
      }
    }
  })
  
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T) & bracketVal1T() == bracketVal8T()) {
        updateTextInput(session, "bracketV8T", label = paste("to the top ", getPercentile(updateGrid(), bracketVal1T() + 10), "%'s wealth above ($m):", sep = ""), value = bracketVal1T() + 10)
      }
    }
  })
  
  
  
  



  ## boundaries for evasion parameter
  observe({
    if (input$evasion != "") {
      if (as.numeric(input$evasion) < 0) {
        updateTextInput(session, "evasion", value = "0")
      }
    }
  })

  observe({
    if (input$evasion != "") {
      if (as.numeric(input$evasion) > 100) {
        updateTextInput(session, "evasion", value = "100")
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
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracket5T)) {
        if (!is.na(bracket5T()) & bracket5T() < 0) {
          updateTextInput(session, "bracket5T", value = 0)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracket6T)) {
        if (!is.na(bracket6T()) & bracket6T() < 0) {
          updateTextInput(session, "bracket6T", value = 0)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracket7T)) {
        if (!is.na(bracket7T()) & bracket7T() < 0) {
          updateTextInput(session, "bracket7T", value = 0)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracket8T)) {
        if (!is.na(bracket8T()) & bracket8T() < 0) {
          updateTextInput(session, "bracket8T", value = 0)
        }
      }
    }
  })

  ## don't let  tax brackets go below 0 or above max wealth after evasion
  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (bracketVal1T() < 0.1) {
      updateTextInput(session, "bracketV1T", value = 0.1)
    }
  })

  observe({
    if (bracketVal1T() > max(updateGrid()$thresNew) / 1e6) {
      updateTextInput(session, "bracketV1T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
      print(max(updateGrid()$thresNew))
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (bracketVal2T() < 0.1) {
      updateTextInput(session, "bracketV2T", value = 0.1)
    }

    observe({
      if (bracketVal2T() > max(updateGrid()$thresNew) / 1e6) {
        updateTextInput(session, "bracketV2T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
        print(max(updateGrid()$thresNew))
      }
    })
  })
  
  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (bracketVal3T() < 0.1) {
      updateTextInput(session, "bracketV3T", value = 0.1)
    }
  })

  observe({
    if (bracketVal3T() > max(updateGrid()$thresNew) / 1e6) {
      updateTextInput(session, "bracketV3T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
      print(max(updateGrid()$thresNew))
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (bracketVal4T() < 0.1) {
      updateTextInput(session, "bracketV4T", value = 0.1)
    }
  })

  observe({
    if (bracketVal4T() > max(updateGrid()$thresNew) / 1e6) {
      updateTextInput(session, "bracketV4T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
      # print(max(updateGrid()$thresNew))
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
        if (bracketVal5T() < 0.1) {
          updateTextInput(session, "bracketV5T", value = 0.1)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 5) {
      if (!is.null(input$bracketV5T)) {
        if (bracketVal5T() > max(updateGrid()$thresNew) / 1e6) {
          updateTextInput(session, "bracketV5T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
        }
      }
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        if (bracketVal6T() < 0.1) {
          updateTextInput(session, "bracketV6T", value = 0.1)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 6) {
      if (!is.null(input$bracketV6T)) {
        if (bracketVal6T() > max(updateGrid()$thresNew) / 1e6) {
          updateTextInput(session, "bracketV6T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
        }
      }
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        if (bracketVal7T() < 0.1) {
          updateTextInput(session, "bracketV7T", value = 0.1)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 7) {
      if (!is.null(input$bracketV7T)) {
        if (bracketVal7T() > max(updateGrid()$thresNew) / 1e6) {
          updateTextInput(session, "bracketV7T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
        }
      }
    }
  })

  ### KATIE: change 0.1s to whatever matches what you put in xval
  ### (1e5 in xval, 1e5/1e6=0.1)
  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        if (bracketVal8T() < 0.1) {
          updateTextInput(session, "bracketV8T", value = 0.1)
        }
      }
    }
  })

  observe({
    if (input$extraBrackets >= 8) {
      if (!is.null(input$bracketV8T)) {
        if (bracketVal8T() > max(updateGrid()$thresNew) / 1e6) {
          updateTextInput(session, "bracketV8T", value = round(max(updateGrid()$thresNew) / 1e6, 0))
        }
      }
    }
  })


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
    as.numeric(input$bracketV1T)
  })
  bracketVal2T <- reactive({
    req(input$bracketV2T)
    as.numeric(input$bracketV2T)
  })
  bracketVal3T <- reactive({
    req(input$bracketV3T)
    as.numeric(input$bracketV3T)
  })
  bracketVal4T <- reactive({
    req(input$bracketV4T)
    as.numeric(input$bracketV4T)
  })
  bracketVal5T <- reactive({
    if (input$extraBrackets >= 5) {
      req(input$bracketV5T)
    }
    as.numeric(input$bracketV5T)
  })
  bracketVal6T <- reactive({
    if (input$extraBrackets >= 6) {
      req(input$bracketV6T)
    }
    as.numeric(input$bracketV6T)
  })
  bracketVal7T <- reactive({
    if (input$extraBrackets >= 7) {
      req(input$bracketV7T)
    }
    as.numeric(input$bracketV7T)
  })
  bracketVal8T <- reactive({
    if (input$extraBrackets >= 8) {
      req(input$bracketV8T)
    }
    as.numeric(input$bracketV8T)
  })




  ## get data ready to plot
  dataInputT <- reactive({
    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))

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

    brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
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

    ## reshuffle to make sure brackets are increasing
    ## tax rates not forced to be monotonic
    reorderIdx <- order(as.numeric(brackets))
    brackets <- brackets[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    ### KATIE: change the 1e5 to whatever you want to be the minimum
    xval <- 10^seq(log10(1e5), log10(45e9), by = 0.001) ## get uniform on log scale

    idx0 <- xval <= as.numeric(brackets[1]) * 1e6
    idx1 <- xval <= as.numeric(brackets[2]) * 1e6 & xval > as.numeric(brackets[1]) * 1e6
    idx2 <- xval > as.numeric(brackets[2]) * 1e6 & xval <= as.numeric(brackets[3]) * 1e6
    idx3 <- xval > as.numeric(brackets[3]) * 1e6 & xval <= as.numeric(brackets[4]) * 1e6

    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6 & xval <= as.numeric(brackets[7]) * 1e6
      idx7 <- xval > as.numeric(brackets[7]) * 1e6 & xval <= as.numeric(brackets[8]) * 1e6
      idx8 <- xval > as.numeric(brackets[8])
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8)
    } else if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6 & xval <= as.numeric(brackets[7]) * 1e6
      idx7 <- xval > as.numeric(brackets[7]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7)
    } else if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6 & xval <= as.numeric(brackets[6]) * 1e6
      idx6 <- xval > as.numeric(brackets[6]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5, idx6)
    } else if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      idx4 <- xval > as.numeric(brackets[4]) * 1e6 & xval <= as.numeric(brackets[5]) * 1e6
      idx5 <- xval > as.numeric(brackets[5]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4, idx5)
    } else {
      idx4 <- xval > as.numeric(brackets[4]) * 1e6
      idx <- cbind.data.frame(idx0, idx1, idx2, idx3, idx4)
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

  ## total tax helper
  totalTax <- reactive({
    ## wait for brackets to be ready
    req(input$bracket1T)
    req(input$bracket2T)
    req(input$bracket3T)
    req(input$bracket4T)
    req(input$bracketV1T)
    req(input$bracketV2T)
    req(input$bracketV3T)
    req(input$bracketV4T)
    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, input$bracket5T)
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T)
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T)
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T, input$bracket8T)
    }
    taxRateP <- as.numeric(taxRate) / 100 ## get to percentage
    bracketStarts <- 1e6 * as.numeric(c(input$bracketV1T, input$bracketV2T, input$bracketV3T, input$bracketV4T))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T), 1e6 * as.numeric(input$bracketV8T))
    }

    reorderIdx <- order(as.numeric(bracketStarts))
    bracketStarts <- bracketStarts[reorderIdx]
    taxRate <- taxRate[reorderIdx]

    taxPerBracket <- getTaxBasePerBracket(updateGrid(), as.numeric(taxRate), as.numeric(bracketStarts))

    taxPerBracket / 1e9 ## in billions
  })

  ## total tax output
  output$totalTax <- renderText({
    round(totalTax())
  })

  ## total tax output over 10 years
  output$totalTax_10 <- renderText({
    totalTax10 <- totalTax() * 13

    round(totalTax10 / 1e3, 2)
  })

  ## total number of households taxed helper
  householdsTaxed <- reactive({
    ## wait for things to be ready
    req(input$bracket1T)
    req(input$bracket2T)
    req(input$bracket3T)
    req(input$bracket4T)
    req(input$bracketV1T)
    req(input$bracketV2T)
    req(input$bracketV3T)
    req(input$bracketV4T)

    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      taxRate <- c(taxRate, input$bracket5T)
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T)
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T)
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      taxRate <- c(taxRate, input$bracket5T, input$bracket6T, input$bracket7T, input$bracket8T)
    }
    taxRateP <- as.numeric(taxRate) / 100 ## get to percentage

    bracketStarts <- 1e6 * as.numeric(c(input$bracketV1T, input$bracketV2T, input$bracketV3T, input$bracketV4T))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      bracketStarts <- c(bracketStarts, 1e6 * as.numeric(input$bracketV5T), 1e6 * as.numeric(input$bracketV6T), 1e6 * as.numeric(input$bracketV7T), 1e6 * as.numeric(input$bracketV8T))
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

  ## use percentile affected
  output$percentTaxUnits <- renderText({
    taxRate <- as.numeric(c(input$bracket1T, input$bracket2T, input$bracket3T, input$bracket4T))

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

    brackets <- as.numeric(c(bracketVal1T(), bracketVal2T(), bracketVal3T(), bracketVal4T()))
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

  
  # F F no
  # F T crash
  # T T crash
  # T F no
  vis2 <- eventReactive(input$submit, ignoreNULL = FALSE, {
    req(input$bracket1T)
    req(input$bracket2T)
    req(input$bracket3T)
    req(input$bracket4T)
    
    req(input$bracketV1T)
    req(input$bracketV2T)
    req(input$bracketV3T)
    req(input$bracketV4T)

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

    # These are mini data set that ggvis needs to create vertical lines
    extra0 <- cbind.data.frame(x = rep(as.numeric(brackets[1]) * 1e6, 2), y = c(0, taxRate[1]))
    extra1 <- cbind.data.frame(x = rep(as.numeric(brackets[2]) * 1e6, 2), y = c(0, taxRate[1]))
    extra1b <- cbind.data.frame(x = rep(as.numeric(brackets[2]) * 1e6, 2), y = c(0, taxRate[2]))
    extra2 <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[2]))
    extra2b <- cbind.data.frame(x = rep(as.numeric(brackets[3]) * 1e6, 2), y = c(0, taxRate[3]))
    extra3 <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[3]))
    extra3b <- cbind.data.frame(x = rep(as.numeric(brackets[4]) * 1e6, 2), y = c(0, taxRate[4]))
    if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
    }
    if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5 <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5b <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[6]))
    }
    if (input$extraBrackets == 7 & !is.null(input$bracket7T)) {
      extra4 <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[4]))
      extra4b <- cbind.data.frame(x = rep(as.numeric(brackets[5]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5 <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[5]))
      extra5b <- cbind.data.frame(x = rep(as.numeric(brackets[6]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6 <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[6]))
      extra6b <- cbind.data.frame(x = rep(as.numeric(brackets[7]) * 1e6, 2), y = c(0, taxRate[7]))
    }
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
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



      row <- data[data$id == x$id, ]

      paste0("Average Tax Rate: ", round(row$marginalRate, 2), "%", " <br> Wealth ($m): ", round(row$xval / 1e6, 0), "<br> Top ", getPercentile(updateGrid(), row$xval / 1e6), "%", "<br> Taxes Paid ($m): ", round(row$marginalInt / 1e6, 2), sep = "") ## dividing by 1e6 may need to change if we do this for xval overall
    }



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
      add_axis("x",
        title_offset = 80, title = "Wealth ($m)", grid = F, format = ",",
        values = brackets, properties = axis_props(labels = list(angle = 45, align = "left", baseline = "middle"))
      ) %>%
      add_axis("y", title = "Tax rate (%)") %>%
      scale_numeric("x", trans = "log", expand = 0) %>%
      set_options(width = 1000, height = 500)
    if (input$extraBrackets == 8 & !is.null(input$bracket8T)) {
      plot %>%
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
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra6b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 6 & !is.null(input$bracket6T)) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra5b, ~ x / 1e6, ~y)
    } else if (input$extraBrackets == 5 & !is.null(input$bracket5T)) {
      plot %>%
        layer_paths(data = extra4, ~ x / 1e6, ~y) %>%
        layer_paths(data = extra4b, ~ x / 1e6, ~y)
    } else {
      plot
    }
  })


  vis2 %>% bind_shiny("plot2")
}