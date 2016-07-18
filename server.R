shinyServer(function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
  })
  

 output$predicted_word <- renderDataTable({
          predictNextWord(input$userWord)
        } , options = list(pageLength = 10, paging = FALSE, searching = FALSE)
 )  
    
})
