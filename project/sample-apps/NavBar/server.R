library(shiny)
library(ggplot2)
vitamins <- readRDS('../data/vitamins.rds')
amino.acids <- readRDS('../data/amino.acids.rds')
lipid.components <- readRDS('../data/lipid.components.rds')
minerals <- readRDS('../data/minerals.rds')
proximates <- readRDS('../data/proximates.rds')
foods <- readRDS('../data/foods.rds')

shinyServer(function(input, output, session) {
  
  # Bar Chart
  output$plot1 <- renderPlot({
    sub <- foods[foods$food_group %in% input$show_vars7,]
    sub$food_group <- reorder(sub$food_group, sub[,input$show_vars6])
    g <- ggplot(sub, aes_string(x = 'food_group', y=input$show_vars6, fill='food_group'))
    g <- g + stat_summary(fun.y = 'mean', geom = 'bar') + coord_flip() +
      theme(legend.position='bottom', axis.title.y = element_blank()) + 
      labs(title = paste('Average', input$show_vars6, sep = ' '), fill = 'Food Groups')
    print(g)},
    height = 600
    )
  
  # Cluster
  selectedData <- reactive({
    foods[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot2 <- renderPlot({
    #par(mar = c(5.1, 4.1, 0, 1))
    #plot(selectedData(),
     #    col = clusters()$cluster,
      #   pch = 20, cex = 3)
    g <- ggplot(selectedData(), aes_string(x=input$xcol, y=input$ycol, color=clusters()$cluster))
    g <- g + geom_point() +
      theme(legend.position='bottom', legend.title=element_blank())
    print(g)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    #legend('topright', inset=0.2, title='Cluster',
     #      c(1:input$clusters), fill=clusters()$cluster, cex=0.8)
  })
  
  output$cluster <- renderTable({
    table(foods$food_group, clusters()$cluster)
  })
  
  # Sub Tables
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      if(length(input$show_vars1) != 0){
        proximates <- proximates[, input$show_vars1, drop = FALSE]
      }
      proximates
    })
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      if(length(input$show_vars2) != 0){
        minerals <- minerals[, input$show_vars2, drop = FALSE]
      }
      minerals
    })
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable({
      if(length(input$show_vars3) != 0){
        vitamins <- vitamins[, input$show_vars3, drop = FALSE]
      }
      vitamins
    })
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable({
      if(length(input$show_vars4) != 0){
        lipid.components <- lipid.components[, input$show_vars4, drop = FALSE]
      }
      lipid.components
    })
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable({
      if(length(input$show_vars5) != 0){
        amino.acids <- amino.acids[, input$show_vars5, drop = FALSE]
      }
      amino.acids
    })
  })
  
  # Download datasets
  datasetInput <- reactive({
    switch(input$dataset,
           "Proximates" = proximates,
           "Vitamins" = vitamins,
           "Minerals" = minerals,
           "Lipid Components" = lipid.components,
           "Amino Acid" = amino.acids,
           "Whole Datasets" = foods)
  })
  
  output$table <- renderTable({
    summary(datasetInput())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  output$home <- renderPrint({
    summary(foods)
  })

})