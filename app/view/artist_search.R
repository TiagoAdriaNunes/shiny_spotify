box::use(
  bslib[input_task_button],
  checkmate[test_data_frame],
  htmltools[tagAppendAttributes],
  memoise[memoise],
  shiny[
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
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
    # Button to trigger search; shows a spinner and is disabled while the
    # search and the cards it updates are loading
    input_task_button(ns("search"), "Search", label_busy = "Searching...", type = "default"),
    # Output to display artist information or error message
    textOutput(ns("artist_info"))
  )
}

# Server function for the module
#' @export
server <- function(id, selected_artist_id, selected_artist_name) {
  moduleServer(id, function(input, output, session) {
    # Defined up front rather than inside the search: an output with no
    # value yet counts as still loading, so the busy spinner would show
    # under the search bar until the first search
    search_message <- reactiveVal("")
    output$artist_info <- renderText(search_message())

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
        search_message("Artist search unavailable. Please try again later.")
      } else if (test_data_frame(artist_result, min.rows = 1)) {
        artist_id <- artist_result$id[1] # Get the first result's artist ID
        artist_name <- artist_result$name[1] # Get the artist name
        # Store the artist ID in the reactive value
        selected_artist_id(artist_id)
        # Store the artist name in the reactive value
        selected_artist_name(artist_name)
        # Display artist's name in the output
        search_message(paste("Found artist:", artist_name))
      } else {
        search_message("Artist not found.")
      }
    })
  })
}
