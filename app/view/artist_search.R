# artist_search.R
box::use(
  bslib[page_fillable],
  memoise[memoise],
  shiny[...], #nolint
  spotifyr[search_spotify],
)

# Memoize the Spotify API functions to enable caching
search_spotify_memo <- memoise(search_spotify)

# UI function for the module
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    verticalLayout(
      # Input for artist name
      textInput(ns("artist_name"), "Enter artist name",
                placeholder = "Type artist name here..."),
      # Button to trigger search
      actionButton(ns("search"), "Search"),
      # Output to display artist information or error message
      textOutput(ns("artist_info"))
    )
  )
}

# Server function for the module
#' @export
server <- function(id, selected_artist_id, selected_artist_name) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$search, {
      req(input$artist_name)  # Ensure artist_name input is not empty
      # Use the memoized version of search_spotify to cache the results
      artist_result <- search_spotify_memo(input$artist_name, type = "artist")
      if (nrow(artist_result) > 0) {
        artist_id <- artist_result$id[1]  # Get the first result's artist ID
        artist_name <- artist_result$name[1]  # Get the artist name
        # Store the artist ID in the reactive value
        selected_artist_id(artist_id)
        # Store the artist name in the reactive value
        selected_artist_name(artist_name)
        # Display artist's name in the output
        output$artist_info <- renderText({
          paste("Found artist:", artist_name)
        })
      } else {
        output$artist_info <- renderText("Artist not found.")
      }
    })
  })
}
