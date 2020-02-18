# Dependencies ----
library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)


# Set up data ----

# > Response table ----
response_filename = "eRum2020-WorkshopSessionsEvaluation (Responses).xlsx"

response_table <- readxl::read_excel(response_filename)

easy_cols <- c("timestamp",
               "ID",
               "reviewer",
               "VOTE",
               "note")

response_table <- setNames(response_table, easy_cols)

response_table <- response_table %>%
  separate(ID, c("id", "title"), sep = "\\|") %>%
  separate(VOTE, c("vote", "meaning", sep = "\\:")) %>%
  mutate(id = gsub("\\.0", "", .$id)) %>%
  mutate(id = gsub("M", "", .$id)) %>%
  mutate(vote = as.numeric(vote)) %>%
  mutate(id = as.numeric(id)) %>%
  filter(!is.na(id)) %>%
  filter(reviewer != "Francesca, Vitalini") %>%
  group_by(id, reviewer) %>%
  distinct()

preselected_cols <- c("id",
                      "title",
                      "vote",
                      "reviewer")

response_table_compact <- response_table[, preselected_cols]

response_table_summarized <- response_table_compact %>%
  group_by(id, title) %>%
  summarise(mean_vote = mean(vote))



# App ----
# > UI definition ----
analyzeR_ui <- fluidPage(

  # >> header definition ----
  fluidRow(column(10, h2("analyzeR - eRum2020"), offset = 1)),

  hr(),

  # >> body definition ----
  fluidRow(
    column(
      width = 4,
      DT::dataTableOutput("DT_response")
    ),
    column(
      width = 8,
      plotlyOutput("plot_votes_id")
    )
  ),
  fluidRow(
    plotlyOutput("plot_votes_summary")
  )
)


# > Server definition ----
analyzeR_server <- function(input, output, session) {

  result <- reactiveValues(
    curr_id = NULL,
    curr_dt_id = NULL
  )

  output$DT_response <- DT::renderDataTable(
    response_table_summarized,
    style = "bootstrap",
    rownames = FALSE,
    filter = "top",
    selection = list(mode = "single"),
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )

  observeEvent(input$DT_response_rows_selected, ignoreNULL = FALSE, {

    print(paste0("input$DT_response_rows_selected is ", input$DT_response_rows_selected))

    # subsets
    s <- input$DT_response_rows_selected
    if (!is.null(s)) {
      result$curr_id =  response_table_summarized[s, "id"]
    }  else {
      result$curr_id = NULL
    }
  })

  output$plot_votes_summary <- renderPlotly(
    ggplotly(ggplot(response_table_summarized, aes(x = as.factor(id), y = mean_vote)) +
      geom_bar(stat = "identity"))
  )

  observeEvent(result$curr_id, ignoreNULL = FALSE, {
    if (!is.null(result$curr_id)) {
      print("updating result$curr_dt_id")
      result$curr_dt_id <- response_table_compact %>%
        filter(id == as.numeric(result$curr_id))
    }
  })

  output$plot_votes_id <- renderPlotly(
    if (!is.null(result$curr_dt_id)) {
      ggplotly(ggplot(result$curr_dt_id, aes( x = as.factor(reviewer), y = vote)) +
        geom_bar(stat = "identity"))
    }
  )

}

# > Run app ----
shinyApp(ui = analyzeR_ui, server = analyzeR_server)
