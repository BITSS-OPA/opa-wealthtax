
server <- function(input, output){
numberTaxpayers <- c(640198,171310,41637,24974,5155,2612,911) ## eventually be able to change these based on other parameters
taxBase <- c(6716, 3510, 2376, 2460, 1285, 660, 2560) ## eventaully be able to change these based on other parameters



bracketNames <- c("$10m-$25m", "$25m-$50m", "$50m-$100m","$100m-$250m","250m-$500m","$500m-$1bn","1bn+")

getMarginalTax <- function(wealth,taxLevels){
  ## expecting taxLevels in percentage
  taxLevels = taxLevels/100
  first = wealth - 10e6 ## first 10 million is free
  
  second = first - 15e6
  third = second - 25e6
  fourth = third - 50e6
  fifth = fourth - 150e6
  sixth = fifth - 250e6
  seventh = sixth - 500e6
 

  firstChunk = ifelse(second>=0,taxLevels[1]*15e6,taxLevels[1]*first)
  secondChunk = ifelse(third>=0,taxLevels[2]*25e6,taxLevels[2]*max(second,0))
  thirdChunk = ifelse(fourth>=0, taxLevels[3]*50e6,taxLevels[3]*max(third,0))
  fourthChunk = ifelse(fifth>=0, taxLevels[4]*150e6, taxLevels[4]*max(fourth,0))
  fifthChunk = ifelse(sixth>=0, taxLevels[5]*250e6,taxLevels[5]*max(fifth,0))
  sixthChunk = ifelse(seventh>=0,taxLevels[6]*500e6,taxLevels[6]*max(sixth,0))
  seventhChunk=ifelse(seventh>=0,seventh*taxLevels[7],0)
  
 toReturn = firstChunk + secondChunk + thirdChunk + fourthChunk + fifthChunk + sixthChunk + seventhChunk
 
 return(toReturn)
}


  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    #yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    #xvar <- prop("x", as.symbol(input$xvar))
    #yvar <- prop("y", as.symbol(input$yvar))
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
    taxRateP <- taxRate/100 ## get to percentage
    
    tax <- taxBase*taxRateP
    
    toPlot = cbind.data.frame(bracketNames,taxRate)
    toPlot$bracketNames <- factor(toPlot$bracketNames,levels=toPlot$bracketNames)
    toPlot %>%
      ggvis(~bracketNames,~taxRate) %>% 
      layer_bars() %>%
      #layer_points(size := 50, size.hover := 200,
      #             fillOpacity := 0.2, fillOpacity.hover := 0.5,
       #            stroke = ~has_oscar, key := ~ID) %>%
      #add_tooltip(movie_tooltip, "hover") %>%
      add_axis("x", title = "Wealth before taxes", title_offset = 75, properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
      add_axis("y", title = "Tax rate (%)") %>%
      #add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      #scale_nominal("stroke", domain = c("Yes", "No"),
       #             range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
 output$totalTax <- renderText({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
    taxRateP <- taxRate/100 ## get to percentage
    
    tax <- taxBase*taxRateP
    
    totalTax <- sum(tax) 
    
   totalTax ## could round if desired
  })
  
  output$totalTaxpayers <- renderText({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
    taxRateP <- taxRate/100 ## get to percentage
    
    tax <- taxBase*taxRateP
    
    householdsTaxed <- numberTaxpayers * (taxRateP>0)
    
    totalTaxpayers = sum(householdsTaxed)
    
    totalTaxpayers
  })
  
  output$percentHouseAffected <- renderText({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
    taxRateP <- taxRate/100 ## get to percentage
    
    tax <- taxBase*taxRateP
    
    householdsTaxed <- numberTaxpayers * (taxRateP>0)
    
    householdsAffected <- sum(householdsTaxed)/129400000 ## make the denominator updateable later
    
    householdsAffected*100 ## get to percentage
    ## can round if desired
  })
  
  output$percentTaxUnits <- renderText({
    taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
    taxRateP <- taxRate/100 ## get to percentage
    
    tax <- taxBase*taxRateP
    
    householdsTaxed <- numberTaxpayers * (taxRateP>0)
    
   taxUnits = sum(householdsTaxed)/183460000 ## make the denominator updateable later
    
   taxUnits*100  ## get to percentage
   ## can round if desired
  })


vis2 <- reactive({
  
  taxRate <- c(input$bracket1, input$bracket2, input$bracket3, input$bracket4, input$bracket5, input$bracket6, input$bracket7)
  
  xval = seq(10e6,1e9+1e8,by=100000)
  idx1 = xval <= 25e6
  idx2 = xval > 25e6 & xval <= 50e6
  idx3 = xval > 50e6 & xval <= 100e6
  idx4 = xval > 100e6 & xval <= 250e6
  idx5 = xval > 250e6 & xval <= 500e6
  idx6 = xval > 500e6 & xval <= 1e9
  idx7 = xval > 1e9
  
  idx = cbind.data.frame(idx1,idx2,idx3,idx4,idx5,idx6,idx7)
  
  getGroup = unlist(apply(idx, 1,function(x){which(x)}) )
  
  toPlot = cbind.data.frame(xval,getGroup)
  
  toMatch = cbind.data.frame(group = 1:7, tax = taxRate)
  
  toPlot2 = merge(toPlot,toMatch,by.x="getGroup",by.y="group")
  
  toPlot2$marginalInt=unlist(lapply(toPlot2$xval,getMarginalTax,taxRate))
  toPlot2$marginalRate=(toPlot2$marginalInt/toPlot2$xval)*100
  
  # don't need group anymore, take out in a sec
  
extra0=cbind.data.frame(x=rep(10e6,2),y=c(0,taxRate[1]))
extra1=cbind.data.frame(x = rep(25e6,2), y =c(0,taxRate[1]))
extra1b=cbind.data.frame(x = rep(25e6,2), y =c(0,taxRate[2]))
extra2=cbind.data.frame(x = rep(50e6,2), y =c(0,taxRate[2]))
extra2b=cbind.data.frame(x = rep(50e6,2), y =c(0,taxRate[3]))

extra3=cbind.data.frame(x = rep(100e6,2), y =c(0,taxRate[3]))
extra3b=cbind.data.frame(x = rep(100e6,2), y =c(0,taxRate[4]))

extra4=cbind.data.frame(x = rep(250e6,2), y =c(0,taxRate[4]))
extra4b=cbind.data.frame(x = rep(250e6,2), y =c(0,taxRate[5]))
extra5=cbind.data.frame(x = rep(500e6,2), y =c(0,taxRate[5]))
extra5b=cbind.data.frame(x = rep(500e6,2), y =c(0,taxRate[6]))
extra6=cbind.data.frame(x = rep(1e9,2), y =c(0,taxRate[6]))
extra6b=cbind.data.frame(x = rep(1e9,2), y =c(0,taxRate[7]))

  
  toPlot2 %>%
    ggvis(x = ~xval, y= ~tax) %>% layer_points() %>% layer_lines(x = ~xval, y=~marginalRate, stroke:="red") %>%
    layer_paths(data = extra1, ~x,~y) %>% 
    layer_paths(data = extra2, ~x, ~y) %>%
    layer_paths(data = extra3, ~x, ~y) %>%
    layer_paths(data = extra4, ~x, ~y) %>%
    layer_paths(data = extra5, ~x, ~y) %>%
    layer_paths(data = extra6, ~x, ~y) %>%
    layer_paths(data = extra0, ~x, ~y) %>%
    layer_paths(data = extra1b, ~x, ~y) %>%
    layer_paths(data = extra2b, ~x, ~y) %>%
    layer_paths(data = extra3b, ~x, ~y) %>%
    layer_paths(data = extra4b, ~x, ~y) %>%
    layer_paths(data = extra5b, ~x, ~y) %>%
    layer_paths(data = extra6b, ~x, ~y) %>%
    
  add_axis("x", title = "Wealth before taxes") %>%
    add_axis("y", title = "Tax rate (%)") %>%
    #add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
    #scale_nominal("stroke", domain = c("Yes", "No"),
    #             range = c("orange", "#aaa")) %>%
    set_options(width = 1000, height = 500)
  # Lables for axes
  #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  #yvar_name <- names(axis_vars)[axis_vars == input$yvar]
  
  # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
  # but since the inputs are strings, we need to do a little more work.
  #xvar <- prop("x", as.symbol(input$xvar))
  #yvar <- prop("y", as.symbol(input$yvar))
})
  
vis2 %>% bind_shiny("plot2")
}