box::use(
  bslib[input_task_button],
  checkmate[
    assert_string,
    test_data_frame
  ],
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
    textOutput,
    updateTextInput
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
    # Button to trigger search; says "Searching..." and is disabled while the
    # search runs. No busy icon: the result cards show the loading spinner.
    input_task_button(ns("search"), "Search", label_busy = "Searching...", icon_busy = NULL, type = "default"),
    # Output to display artist information or error message
    textOutput(ns("artist_info"))
  )
}

# Server function for the module
#' @return A function `open_artist(name)` that searches for `name` exactly as
#'   if it had been typed and searched, showing it in the search box. It
#'   returns TRUE when an artist was found and selected, FALSE otherwise.
#' @export
server <- function(id, selected_artist_id, selected_artist_name) {
  moduleServer(id, function(input, output, session) {
    # Defined up front rather than inside the search: an output with no
    # value yet counts as still loading, so the busy spinner would show
    # under the search bar until the first search
    search_message <- reactiveVal("")
    output$artist_info <- renderText(search_message())

    # Searches Spotify for `name` and selects the first matching artist.
    # Returns TRUE when one was found.
    search_artist <- function(name) {
      # Use the memoized version of search_spotify to cache the results
      search_failed <- FALSE
      artist_result <- tryCatch(
        search_spotify_memo(name, type = "artist"),
        error = function(e) {
          warning("Spotify search failed: ", conditionMessage(e), call. = FALSE)
          search_failed <<- TRUE
          NULL
        }
      )
      if (search_failed) {
        search_message("Artist search unavailable. Please try again later.")
        return(FALSE)
      }
      if (!test_data_frame(artist_result, min.rows = 1)) {
        search_message("Artist not found.")
        return(FALSE)
      }
      artist_name <- artist_result$name[1]
      selected_artist_id(artist_result$id[1])
      selected_artist_name(artist_name)
      search_message(paste("Found artist:", artist_name))
      TRUE
    }

    observeEvent(input$search, {
      req(input$artist_name) # Ensure artist_name input is not empty
      search_artist(input$artist_name)
    })

    # Used from other tabs (e.g. clicking an artist in the genre results):
    # shows the name in the search box so it's clear what was searched
    function(name) {
      assert_string(name, min.chars = 1)
      updateTextInput(session, "artist_name", value = name)
      search_artist(name)
    }
  })
}
