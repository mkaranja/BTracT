library(shinyWidgets)

tissue_culture_page = function() tagList(
 fluidRow(
    uiOutput("newWindowContent", style = "display: none;"),
    tags$script(HTML("
      $(document).ready(function() {
        if(window.location.hash != '') {
          $('div:not(#newWindowContent)').hide();
          $('#newWindowContent').show();
          $('#newWindowContent').appendTo('body');
        }
      })
    ")),
    a(href = "#Labels", target = "blank",
      actionBttn("tc", "Tissue Culture Barcode Labels Management", style = "stretch", color = "success", size = "sm")
    )
  )
)
