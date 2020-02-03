library(shiny)
library(shinydashboard)
library(clipr)
library(rintrojs)

assessr <- function(abstract_filename = "erum2020_sessions_for_reviewers.xlsx") {
  
  abstract_table <- readxl::read_excel(abstract_filename)
  
  preselected_cols <- c("Id",
                        "Title",
                        "Session format",
                        "Track",
                        "Keywords (1-3)",
                        "Link",
                        "First time presenting this?",
                        "Speaker Notes")
  
  abstract_table_compact <- 
    abstract_table[, preselected_cols]
  
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
          inputId = "cols_abstract",
          label = "Columns to display",
          choices = colnames(abstract_table),
          selected =  preselected_cols, 
          multiple = TRUE
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
          uiOutput("session_abstract"),
        )
      )
    )
  )
  
  
  # Server definition -------------------------------------------------------
  assessr_server <- function(input, output, session) {
    
    current_dt <- reactive({
      mydt <- abstract_table[, input$cols_abstract]
      return(mydt)
    })
    
    output$session_abstract <- renderUI({
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(h3("Select an abstract from the table to display the full info"))
      
      this_submission <- abstract_table[s, ]
      this_title <- this_submission$Title
      this_abstract <- this_submission$Description
      this_track <- this_submission$Track
      this_format <- this_submission$`Session format`
      this_keywords <- this_submission$`Keywords (1-3)`
      this_link <- this_submission$Link
      this_firsttime <- this_submission$`First time presenting this?`
      this_notes <- this_submission$`Speaker Notes`

      return(
        tagList(
          h3("Title: "),
          tags$b(this_title),
          h3("Abstract: "),
          tags$p(this_abstract),
          h3("Track:"),
          p(this_track),
          h4("Format:"),
          p(this_format),
          h4("Keywords:"),
          tags$b(this_keywords),
          h4("Link:"),
          p(this_link),
          h4("First time presenting this?"), 
          p(ifelse(this_firsttime=="Checked",yes = "Yes", no = "No")),
          h4("Speaker notes:"),
          p(this_notes),
          
          shiny::actionButton(
            inputId = "launch_gform", label = "Open the Google Form to insert your evaluation", 
            icon = icon("database"), 
            onclick ="window.open('TODOTODO_LINK', '_blank')",
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
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50, 100, nrow(current_dt()))
        )
      )
    })
    
    observeEvent(input$launch_gform, {
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(NULL)
      
      # TODO: return id|title, to be copied in one go
      this_submission <- abstract_table[s, ]
      this_id <- this_submission$Id
      this_title <- this_submission$Title
      
      this_toclipboard <- paste(this_id, this_title, sep = "|")
      clipr::write_clip(this_toclipboard)
      showNotification(
        paste0(
          "Copying the id of the abstract to the clipboard... just paste it in the Google Form!",
          this_toclipboard
        ),
        duration = 10, type = "message")
    })
    
    observeEvent(input$tour_assessr, {
      tour <- read.delim("tour_info.txt",
                         sep = ";", stringsAsFactors = FALSE,
                         row.names = NULL, quote = "")
      rintrojs::introjs(session, options = list(steps = tour))
    })
    
  }  
  
  shinyApp(ui = assessr_ui, server = assessr_server)     
}       


