
library(shinyWidgets)
library(shinyFeedback)



feedbackserver <- function(env_serv) with(env_serv, local({
  
  # FEEDBACK TAB
  # validate email address
  
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  }
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled )
  })

# Gather all the form inputs (and add timestamp)
formData <- reactive({

  data <- sapply(fieldsAll, function(x) input[[x]])
  data <- c(data, Timestamp = epochTime())
  data <- t(data)
  data
})    

# When the Submit button is clicked, submit the response
observeEvent(input$submit, {
  
  # User-experience stuff
  shinyjs::disable("submit")
  shinyjs::show("submit_msg")
  shinyjs::hide("error")
  
  # Save the data (show an error message in case of error)
  tryCatch({
    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable("submit")
    shinyjs::hide("submit_msg")
  })
})

# submit another response
observeEvent(input$submit_another, {
  shinyjs::show("form")
  shinyjs::hide("thankyou_msg")
})

# render the admin panel
output$adminPanelContainer <- renderUI({
  if (!isAdmin()) {
    return(NULL)
  } else {
    
    div(
      id = "adminPanel",
      tags$p(style = "color: orange; font-size: 18px; text-align: center;","Feedback (only visible to admins)"),
      DT::dataTableOutput("feedbackTable"),br(),
      downloadBttn("downloadBtn", "Download"), br()
      
    )
  }
  
})

# validate email address
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

# determine if current user is admin
isAdmin <- reactive({
  is.null(session$user) || session$user %in% adminUsers
})    

# Show the feedback in the admin table
observeEvent(input$feedback, {
  showModal(modalDialog(
    feedback(),
    size = "l",
    easyClose = TRUE,
    footer = NULL
  ))
})



output$feedbackTable <- DT::renderDataTable(
  loadData(),
  rownames = FALSE,
  options = list(searching = FALSE, lengthChange = FALSE)
)

# Allow user to download feedback
output$downloadBtn <- downloadHandler(
  filename = function() { 
    sprintf("btract_feedback_%s.csv", humanTime())
  },
  content = function(file) {
    write.csv(loadData(), file, row.names = FALSE)
  }
)
}))