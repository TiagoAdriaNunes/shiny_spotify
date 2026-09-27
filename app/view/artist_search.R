box::use(
  checkmate[test_data_frame],
  htmltools[tagAppendAttributes],
  memoise[memoise],
  shiny[
    actionButton,
    moduleServer,
    NS,
    observeEvent,
    renderText,
    req,
    tags,
    textInput,
    textOutput
  ],
)
box::use(
  app / logic / spotify_api[search_spotify],
)

# Memoize the Spotify API functions to enable caching
search_spotify_memo <- memoise(search_spotify)

# UI function for the module
#' @export
ui <- function(id) {
  ns <- NS(id)
  # A single row so the search sits above the result cards
  tags$div(
    class = "artist-search",
    # Input for artist name. A visible <label> would push the input below
    # the button, so it's named for screen readers with aria-label instead
    textInput(ns("artist_name"), label = NULL, placeholder = "Enter artist name...") |>
      tagAppendAttributes(`aria-label` = "Artist name", .cssSelector = "input"),
    # Button to trigger search
    actionButton(ns("search"), "Search"),
    # Output to display artist information or error message
    textOutput(ns("artist_info"))
  )
}

# Server function for the module
#' @export
server <- function(id, selected_artist_id, selected_artist_name) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$search, {
      req(input$artist_name) # Ensure artist_name input is not empty
      # Use the memoized version of search_spotify to cache the results
      search_failed <- FALSE
      artist_result <- tryCatch(
        search_spotify_memo(input$artist_name, type = "artist"),
        error = function(e) {
          warning("Spotify search failed: ", conditionMessage(e), call. = FALSE)
          search_failed <<- TRUE
          NULL
        }
      )
      if (search_failed) {
        output$artist_info <- renderText("Artist search unavailable. Please try again later.")
      } else if (test_data_frame(artist_result, min.rows = 1)) {
        artist_id <- artist_result$id[1] # Get the first result's artist ID
        artist_name <- artist_result$name[1] # Get the artist name
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
