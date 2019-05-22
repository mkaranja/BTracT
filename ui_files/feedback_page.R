

# Feedback help functions
# which fields get saved 
fieldsAll <- c("Name", "Email", "Organization", "Message")

# which fields are mandatory
fieldsMandatory <- c("Name", "Email","Organization")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}
 
# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(feedbackDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all feedback into a data.frame
loadData <- function() {
  files <- list.files(file.path(feedbackDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where feedback get stored
feedbackDir <- file.path("feedback")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# usernames that are admins
adminUsers <- c("margaret","trushar")
# ---------end of feedback help functions ----------------------------------
feedback = function() tagList(
  
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Feedback message",
    
        column(6, offset = 2,
           
              fluidRow(
                     div(
                         id = "form",
                         tags$p(style = "color: orange; font-size: 24px; text-align: center;",
                                "Leave your feedback or suggestion here"),
                         useShinyFeedback(), 
                         column(6,
                           textInput("Name", labelMandatory("Your name"),width = '100%', placeholder = "Full Name"),
                           textInput("Email", labelMandatory("Your email"),width = '100%', placeholder = "Email address"),
                           textInput("Organization", labelMandatory("Organization"),width = '100%', placeholder = "Company")
                           ),
                         column(6,
                           textAreaInput("Message", "Message/ comment", width = '600px', height = '250px', resize = "both", placeholder = "Message")
                           ),
                         column(1, offset = 12,
                          actionBttn("submit", "Submit", style = "material-flat", size = "sm")
                         ),
                         
                         
                         shinyjs::hidden(
                           span(id = "submit_msg", "Submitting..."),
                           div(id = "error",
                               div(br(), tags$b("Error: "), span(id = "error_msg"))
                           )
                         )
                       ),
                       
                       shinyjs::hidden(
                         div(
                           id = "thankyou_msg",
                           h3("Thanks, your response was submitted successfully!"),
                           actionLink("submit_another", "Submit another response")
                         )
                       )
                ), br(), br(),
                   hr(), br(),
                  uiOutput("adminPanelContainer")
    )
  )
)

