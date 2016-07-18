shinyUI(fluidPage(
  titlePanel("Dynamically generated next word based on prediction algo"),
  sidebarPanel(
    helpText('Instructions:
              Enter a sentence with at least 3 words and press the submit button. 
              The results of predicted words will be displayed. 
              You could choose one of them and submit again to see results.
             e.g. to try
              * president
              * parking lot
              * new york or new york city
              * going to the
              some of the sentence return no matches. So would keep trying with four words or more to see results.')
  ),
  fluidRow(
    sidebarPanel(
    textInput("userWord", "Enter your Words",value = "Enter Text"),
    submitButton("Submit"),
    br()),

    mainPanel(
      tags$p("Predicted Word:"),
      #verbatimTextOutput("predicted_word")
      dataTableOutput("predicted_word")
      )
  )
))
