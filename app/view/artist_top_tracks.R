box::use(
  htmltools[tagList],
  shiny[...],#nolint
  spotifyr[get_artist_top_tracks],
)

# UI function for the artist's top tracks
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Top Tracks"),
    uiOutput(ns("top_tracks_list"))
  )
}

# Server function for the artist's top tracks
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use session to define ns within the server
    # Observe changes in artist_id (reactive)
    observeEvent(artist_id(), {
      if (is.null(artist_id()) || artist_id() == "") {
        # No artist_id is available yet
        output$top_tracks_list <- renderUI({
          tags$p("Please select an artist to see their top tracks.")
        })
        return()
      }
      # If artist_id is available, proceed with fetching top tracks
      req(artist_id())  # Ensure artist_id is not empty or invalid
      # Fetch top tracks for the artist using the provided artist_id
      top_tracks <- get_artist_top_tracks(artist_id())
      output$top_tracks_list <- renderUI({
        if (is.null(top_tracks) || nrow(top_tracks) == 0) {
          return(tags$p("No top tracks found."))
        }
        # Display the top 5 tracks
        tagList(
          tags$h3("Top Tracks"),
          tags$ul(
            lapply(1:min(5, nrow(top_tracks)), function(i) {
              tags$li(paste(top_tracks$name[i], "-", top_tracks$popularity[i], "popularity"))
            })
          )
        )
      })
    })
  })
}
