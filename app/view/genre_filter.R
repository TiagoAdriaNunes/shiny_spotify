# genre_filter.R
box::use(
  app/config/genres[genres_list],
  dplyr[`%>%`, select],
  memoise[memoise],
  shiny[...],
  spotifyr[get_genre_artists],
  reactable[colDef, reactable, reactableOutput, renderReactable, reactableTheme]
)

# Memoized function for caching API calls
get_genre_artists_memo <- memoise(get_genre_artists)

# UI function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Find Artists by Genre"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          ns("genre"), "Select Genre",
          choices = c("", genres_list),
          selected = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select a genre"
          )
        ),
        actionButton(ns("search"), "Search")
      ),
      mainPanel(
        reactableOutput(ns("artist_table")),
        textOutput(ns("message"))
      )
    )
  )
}

# Server function
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$search, {
      req(input$genre)
      artist_results <- tryCatch({
        # Default limit set to 50 as API only accept this max value
        get_genre_artists_memo(genre = input$genre, limit = 50)
      }, error = function(e) {
        output$message <- renderText({ paste("An error occurred:", e$message) })
        NULL
      })
      if (is.null(artist_results) || nrow(artist_results) == 0) {
        output$artist_table <- renderReactable({ NULL })
        output$message <- renderText({
          paste("No artists found for the genre '", input$genre, "'. Please try a different genre.")
        })
      } else {
        output$message <- renderText({ "" })
        output$artist_table <- renderReactable({
          reactable(
            artist_results %>% select(name, popularity, followers.total),
            columns = list(
              name = colDef(name = "Name"),
              popularity = colDef(name = "Popularity"),
              followers.total = colDef(name = "Followers")
            ),
            theme = reactableTheme(
              backgroundColor = "#2B2B2B",
              color = "#E0E0E0",
              borderColor = "#444444",
              headerStyle = list(
                backgroundColor = "#1F1F1F",
                color = "#E0E0E0"
              ),
              tableBodyStyle = list(
                backgroundColor = "#2B2B2B"
              ),
              rowHighlightStyle = list(
                backgroundColor = "#3A3A3A"
              ),
              paginationStyle = list(
                backgroundColor = "#1F1F1F",
                color = "#E0E0E0"
              ),
              pageButtonHoverStyle = list(
                backgroundColor = "#3A3A3A"
              )
            ),
            pagination = TRUE,
            paginationType = "simple",
            defaultPageSize = 10,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 20, 50)
          )
        })
      }
    })
  })
}
