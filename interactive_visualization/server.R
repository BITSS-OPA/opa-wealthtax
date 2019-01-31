
server <- function(input, output){
numberTaxpayers <- c(640198,171310,41637,24974,5155,2612,911) ## eventually be able to change these based on other parameters
taxBase <- c(6716, 3510, 2376, 2460, 1285, 660, 2560) ## eventaully be able to change these based on other parameters



bracketNames <- c("$10m-$25m", "$25m-$50m", "$50m-$100m","$100m-$250m","250m-$500m","$500m-$1bn","1bn+")




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
  
extra0=cbind.data.frame(x=rep(10e6,2),y=c(0,taxRate[1]))
extra1=cbind.data.frame(x = rep(25e6,2), y =c(0,taxRate[1]))
extra2=cbind.data.frame(x = rep(50e6,2), y =c(0,taxRate[2]))
extra3=cbind.data.frame(x = rep(100e6,2), y =c(0,taxRate[3]))
extra4=cbind.data.frame(x = rep(250e6,2), y =c(0,taxRate[4]))
extra5=cbind.data.frame(x = rep(500e6,2), y =c(0,taxRate[5]))
extra6=cbind.data.frame(x = rep(1e9,2), y =c(0,taxRate[6]))
  
  toPlot2 %>%
    ggvis(x = ~xval, y= ~tax) %>% layer_points() %>% layer_lines() %>%
    layer_paths(data = extra1, ~x,~y) %>% 
    layer_paths(data = extra2, ~x, ~y) %>%
    layer_paths(data = extra3, ~x, ~y) %>%
    layer_paths(data = extra4, ~x, ~y) %>%
    layer_paths(data = extra5, ~x, ~y) %>%
    layer_paths(data = extra6, ~x, ~y) %>%
    layer_paths(data = extra0, ~x, ~y) %>%
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