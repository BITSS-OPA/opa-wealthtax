
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
  
}