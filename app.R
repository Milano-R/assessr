library(shiny)
library(shinydashboard)
library(clipr)
library(rintrojs)


workshop_form <- function(id) {
  paste0("https://docs.google.com/forms/d/e/1FAIpQLScTMLTJ1ccfBmjdEPhZfk5CyQwqSAW5AUJyDkFxc7Q9ZW6VPQ/viewform?usp=pp_url&entry.840333480=",
         id)
}

contribution_form <- function(id) {
  paste0("https://docs.google.com/forms/d/e/1FAIpQLSezGbJ1JmgOwDI5BLl28gXp3YQfoFXq8GoMon3k9PZcePCF_w/viewform?usp=pp_url&entry.840333480=",
         id)
}
window_open <- function(form_link) {
  paste0("window.open('", form_link, "', '_blank')")
}


window_open_erum <- function(id, type) {
  if (type == "workshop") {
    link <- workshop_form(id)
  } else if (type == "contribution") {
    link <- contribution_form(id)
  }
  
  window_open(link)
}



  workshop_path <- "./erum2020_sessions_allcontribs_fullinfo_onlyWorkshops.xlsx"
  contribution_path <- "./erum2020_sessions_allcontribs_fullinfo_noWorkshops.xlsx"
  
  workshop_table <- readxl::read_excel(workshop_path)
  contribution_table <- readxl::read_excel(contribution_path)
  
  
  reviewers_names <- unique(c(workshop_table$Reviewer1, workshop_table$Reviewer2,
                              workshop_table$Reviewer3))
  
  
  abstract_table_compact <- 
    workshop_table[, preselected_cols]
  

  
  preselected_cols <- c("Id",
                        "Title",
                        "Session format",
                        "Track",
                        "Keywords (1-3)",
                        "Link",
                        "First time presenting this?",
                        "Speaker Notes")
  
  
  # UI definition -----------------------------------------------------------
  assessr_ui <- shinydashboard::dashboardPage(
    skin = "black",
    rintrojs::introjsUI(),
    # header definition -------------------------------------------------------
    header = shinydashboard::dashboardHeader(
      title = "assessr - eRum2020",
      titleWidth = 350
    ),
    
    # sidebar definition ------------------------------------------------------
    sidebar = shinydashboard::dashboardSidebar(
      width = 250,
      shinydashboard::menuItem(
        text = "Sessions Settings", icon = icon("cog"),
        startExpanded = TRUE,
        selectInput(
          inputId = "submissionType",
          label = "Type of contribution",
          choices = c("Workshops" = "workshop", "Talks and other sessions" = "contribution"),
          selected =  "Workshop", 
          multiple = FALSE
        ),
        selectInput(
          inputId = "cols_abstract",
          label = "Columns to display",
          choices = colnames(workshop_table),
          selected =  preselected_cols, 
          multiple = TRUE
        ),
        selectInput(
          inputId = "reviewer",
          label = "Reviewer name",
          choices = c("All", reviewers_names),
          selected =  "All", 
          multiple = FALSE
        ),
        
        
        actionButton(
          inputId = "tour_assessr",
          icon = icon("question-circle"),
          label = "How does assessr work?",
          class = "btn-info"
        )
      )
    ),
    
    # body definition ---------------------------------------------------------
    body = shinydashboard::dashboardBody(
      id = "main-app",
      
      fluidRow(
        column(
          width = 6,
          DT::dataTableOutput("DT_abstracts")
        ),
        column(
          width = 6,
          hr(),
          uiOutput("session_abstract")
        )
      )
    )
  )
  
  
  # Server definition -------------------------------------------------------
  assessr_server <- function(input, output, session) {
    
    
    
    abstract_table <- reactive({
      if (input$submissionType == "workshop") {
        abstract_t <- workshop_table
      } else if (input$submissionType == "contribution") {
        abstract_t <- contribution_table
      }
      
      abstract_t
    })
    
    current_dt <- reactive({
      
      mydt <- abstract_table()[abstract_table()$Reviewer1 %in% input$reviewer |
                                 abstract_table()$Reviewer2 %in% input$reviewer |
                                 abstract_table()$Reviewer3 %in% input$reviewer |
                                 "All" %in% input$reviewer,
                               input$cols_abstract]
      return(mydt)
    })
    
    output$session_abstract <- renderUI({
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(h3("Select an abstract from the table to display the full info"))
      
      this_submission <- abstract_table()[s, ]
      this_title <- this_submission$Title
      this_id <- this_submission$Id
      this_abstract <- this_submission$Description
      this_track <- this_submission$Track
      this_format <- this_submission$`Session format`
      this_keywords <- this_submission$`Keywords (1-3)`
      this_link <- this_submission$Link
      this_firsttime <- this_submission$`First time presenting this?`
      this_notes <- this_submission$`Speaker Notes`
      
      
      # form_id:
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(NULL)
      
      this_submission <- abstract_table()[s, ]
      this_id <- this_submission$Id
      this_title <- this_submission$Title
      
      form_id <- gsub( "'", "", paste(this_id, this_title, sep = "|"))
      
      message("type: ", input$submissionType)
      message("form_id(): ", form_id)
 #     message("open link string: ", open_link_string)
      
      return(
        tagList(
          h3("Title: "),
          tags$b(h2(this_title)),
          h4("Contribution identifier: "),
          tags$p(paste(this_id, this_title, sep = "|")),
          h3("Abstract: "),
          tags$p(this_abstract),
          h3("Track:"),
          p(this_track),
          h3("Format:"),
          p(this_format),
          h3("Keywords:"),
          tags$b(this_keywords),
          h3("Link:"),
          p(this_link),
          h3("First time presenting this?"), 
          p(ifelse(this_firsttime=="Checked",yes = "Yes", no = "No")),
          h3("Speaker notes:"),
          p(this_notes),
          
          shiny::actionButton(
            inputId = "launch_gform", label = "Open the Google Form to insert your evaluation", 
            icon = icon("database"), 
            onclick =window_open_erum(form_id, input$submissionType),
            class = "btn-success"
          )
        )
      )
      
    })
    
    output$DT_abstracts <- DT::renderDataTable({
      DT::datatable(
        current_dt(),
        style = "bootstrap", 
        rownames = FALSE, 
        filter = "top",
        selection = list(mode = "single"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 10, 25, 50, 100, nrow(current_dt()))
        )
      )
    })
    
    observeEvent(input$tour_assessr, {
      tour <- read.delim("tour_info.txt",
                         sep = ";", stringsAsFactors = FALSE,
                         row.names = NULL, quote = "")
      rintrojs::introjs(session, options = list(steps = tour))
    })
    
  }  
  
  shinyApp(ui = assessr_ui, server = assessr_server)
  