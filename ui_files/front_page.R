frontpage = function() tagList(
  tags$style(".topimg {
                            margin-left:-30px;
                            margin-right:-30px;
                            margin-top:-15px;
                          }"),
  div(class="topimg",img(src="BTracTImg.png", width="100%")),
  fluidRow(
    column(6, offset = 3,
      tags$p(class = "intro", "The BTracT Dashboard is an analytic tool for information collected using BTracT (ODK). Information is presented using dynamic graphs and data tables."),
      #div(class = "intro-divider"),
      br(),
      tags$p("Main subject-area groupings of BTracT data are shown on the toolbar above. To navigate around this site, left-click one of the tabs and for tabs with categories choose the drop down list."),
      br(),
      tags$p("Graphs can be downloaded as png and tables can be downloaded to a csv file. Information on how to use the graphs and tables, along with an example, is available in the",
             tags$a("Help", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
             tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
        ),
      br(),
      tags$img(class = "rear-preview", src = "odk.png"), br(),
      
      tags$p("BTracT is an ",span(class = "bold", "automated"),
                  "system for tracking banana crosses and seeds. All accessions are designated with unique barcode identifier.
                   This system ensures reduced number of errors while also providing instant access to reports"),
                   
      tags$p(tags$span(class = "bold", "PLEASE NOTE:"),
                   "This webpage may time-out if left idle too long, which will cause the screen to grey-out.",
                   "To use the webpage again, refresh the page. This will reset all previously-selected input options."),
      br(), br(),br(), br(),
      BTracTFooter(),
      br()
      )
  )
)
  