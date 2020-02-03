# Dependencies ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)

# Set up data ----

# > Abstracts table ----
abstract_filename = "erum2020_sessions_for_reviewers.xlsx"

abstract_table <- readxl::read_excel(abstract_filename)

preselected_cols <- c("Id",
                      "Title",
                      "Session format",
                      "Track",
                      "Keywords (1-3)")

abstract_table_compact <- abstract_table[, preselected_cols] %>%
  mutate("Track Leader" = "",
         "Reviewer 1" = "",
         "Reviewer 2" = "")

# > Track renaming ----
tracks <- data.frame(
  name = c("dataviz_shiny", "applications", "r_prod", "r_world", "ml_models", "life_sciences"),
  descs = c("R Dataviz & Shiny", "R Applications", "R Production", "R World", "R Machine Learning & Models", "R Life Sciences"),
  stringsAsFactors = FALSE
)


# > Track leaders ----
trackleaders <- data.frame(
  name = c("dataviz_shiny", "applications", "r_prod", "r_world", "ml_models", "life_sciences"),
  leader = c("Mariachiara Fortuna", "Emanuela Furfaro", "Andrea Melloncelli", "Roberta Sirovich", "Filippo Chiarello", "Riccardo Rossi"),
  stringsAsFactors = FALSE
)

trakleaders <- left_join(trackleaders, tracks)

# > Track committee ----
trackcommittee <- list(
  "applications" = c("Adolfo Alvarez", "Stefano Iacus", "Enrico Deusebio", "Pier Luca Lanzi"),
  "dataviz_shiny" = c("Olga Sulima", "Diane Beldame"),
  "r_world" = c("Martin Machler", "Xavier Adam", "Hannah Frick"),
  "r_prod" = c("Andrie De Vries", "Goran Milovanovic", "Gergely Daroczi"),
  "ml_models" =  c("Heather Turner", "Branko Kovac", "Piercesare Secchi", "Aldo Solari", "Fulvia Pennoni"),
  "life_sciences" = c("Davide Cittaro", "Davide Risso", "Levi Waldron", "Charlotte Soneson")
)

# Full tracks infos ----
track_info <- trakleaders %>%
  mutate(committee = trackcommittee[trakleaders$name])

# App ----
# > UI definition ----
assignR_ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs

  # >> header definition ----
  fluidRow(column(10, h2("assignR - eRum2020"), offset = 1)),

  hr(),

  # >> body definition ----
  fluidRow(
    column(
      width = 8,
      DT::dataTableOutput("DT_abstracts")
    ),
    column(
      width = 4,
      uiOutput("session_tracks"),
      hr(),
      uiOutput("session_track_keywords"),
      hr(),
      uiOutput("session_track_leaders"),
      actionButton("pick_track_leader", label = "Assign Track Leader"),
      hr(),
      uiOutput("session_track_committee"),
      disabled(actionButton("pick_reviewer1", label = "Select Reviewer1")),
      disabled(actionButton("pick_reviewer2", label = "Select Reviewer2")),
      hr(),
      downloadButton("save", "save")
    )
  )
)


# > Server definition ----
assignR_server <- function(input, output, session) {

  result <- reactiveValues(
    current_dt = abstract_table_compact,
    tracks = "",
    track_keywords = "",
    track_leaders = "",
    track_committee = "",
    curr_track_committee = ""
  )

  output$DT_abstracts <- DT::renderDataTable(
    result$current_dt,
    style = "bootstrap",
    rownames = FALSE,
    filter = "top",
    selection = list(mode = "single"),
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 25, 50, 100, nrow(result$current_dt))
    )
  )

  observeEvent(input$DT_abstracts_rows_selected, ignoreNULL = FALSE, {

    print(paste0("input$DT_abstracts_rows_selected is ", input$DT_abstracts_rows_selected))

    # subsets
    s <- input$DT_abstracts_rows_selected
    table <-  result$current_dt
    if (!is.null(s)) {
      row <- table[s, ]
      tracks_val <- row %>%
        select(Track) %>%
        as.character() %>%
        strsplit(split = ", ") %>%
        unlist() %>%
        unique()
      track_keywords_val <- row %>%
        select('Keywords (1-3)') %>%
        as.character() %>%
        strsplit(split = ", ") %>%
        unlist() %>%
        unique()
      selected_track_info_val <- track_info %>%
        filter(descs %in% tracks_val)
      track_leaders_val <- selected_track_info_val %>%
        select(leader) %>%
        unlist() %>%
        as.character()
      track_committee_val <- selected_track_info_val %>%
        select(committee) %>%
        unlist() %>%
        as.character() %>%
        unique()

      result$tracks <- tracks_val
      result$track_keywords <- track_keywords_val
      result$track_leaders <- track_leaders_val
      result$track_committee <- track_committee_val
    } else {
      result$tracks <- ""
      result$track_keywords <- ""
      result$track_leaders <- ""
      result$track_committee <- ""
    }
  })

  output$session_tracks <- renderUI({
    textInput("tracks", "Tracks", value = paste0( result$tracks, collapse = ", "))
  })
  output$session_track_keywords <- renderUI({
    textInput("track_keywords", "Keywords", value = paste0( result$track_keywords, collapse = ", "))
  })
  output$session_track_leaders <- renderUI({
    tagList(
      textInput("track_leaders", "Track Leaders", value = paste0(result$track_leaders, collapse = ", ")),
    selectInput("track_leader", "Track Leader", choices =  result$track_leaders, selected = NULL)
    )
  })
  output$session_track_committee <- renderUI({
    tagList(
      textInput("track_committee", "Reviewers", value = paste0(result$track_committee, collapse = ", ")),
      selectInput("reviewer1", "Reviewer 1", choices =  result$curr_track_committee, selected = NULL),
      selectInput("reviewer2", "Reviewer 2", choices =  result$curr_track_committee, selected = NULL)
    )
  })

  observeEvent(input$track_leaders, ignoreNULL = FALSE,{
    if (!is.null(input$track_leaders)) {
      result$curr_track_committee <- track_info %>%
        filter(leader %in% input$track_leaders) %>%
        select(committee) %>%
        unlist() %>%
        as.character() %>%
        unique()
    }
  })

  observeEvent({input$reviewer1
    input$DT_abstracts_rows_selected}, ignoreNULL = FALSE, {
    if (! is.null(input$DT_abstracts_rows_selected) && !is.null(input$reviewer1)) {
      enable("pick_reviewer1")
    } else {
      disable("pick_reviewer1")
    }
  })

  observeEvent({input$reviewer2
    input$DT_abstracts_rows_selected}, ignoreNULL = FALSE, {
    if (!is.null(input$DT_abstracts_rows_selected) && !is.null(input$reviewer2) && input$reviewer2 != input$reviewer1) {
      enable("pick_reviewer2")
    } else {
      disable("pick_reviewer2")
    }
  })

  observeEvent({input$track_leader
    input$DT_abstracts_rows_selected}, ignoreNULL = FALSE, {
      if (!is.null(input$DT_abstracts_rows_selected) && !is.null(input$track_leader)) {
        enable("pick_track_leader")
      } else {
        disable("pick_track_leader")
      }
  })

  observeEvent(input$pick_track_leader, {
    dt_update <-  result$current_dt
    s <- input$DT_abstracts_rows_selected
    dt_update[s, 'Track Leader'] <- input$track_leader
    result$current_dt <- dt_update
  })

  observeEvent(input$pick_reviewer1, {
    dt_update <-  result$current_dt
    s <- input$DT_abstracts_rows_selected
    dt_update[s, 'Reviewer 1'] <- input$reviewer1
    result$current_dt <- dt_update
  })

  observeEvent(input$pick_reviewer2, {
    dt_update <- result$current_dt
    s <- input$DT_abstracts_rows_selected
    dt_update[s, 'Reviewer 2'] <- input$reviewer2
    result$current_dt <- dt_update
  })

  output$save <- downloadHandler(
    filename = "assigned.csv",
    content = function(file){
      data.table::fwrite(result$current_dt, file = file, row.names = FALSE, quote = TRUE)
    }
  )

}

# > Run app ----
shinyApp(ui = assignR_ui, server = assignR_server)
