box::use(
  htmltools[tagList],
  memoise[memoise],
  shiny[...], #nolint
  spotifyr[get_artist_top_tracks],
)

# Memoize the get_artist_top_tracks function to cache the results
get_artist_top_tracks_memoized <- memoise(get_artist_top_tracks)

# UI function for the artist's top tracks
ui <- function(id) { #nolint
  ns <- NS(id)
  tagList(
    tags$h3("Top Tracks"),
    uiOutput(ns("top_tracks_list")),
  )
}

# Server function for the artist's top tracks
server <- function(id, artist_id) { #nolint
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
      top_tracks <- get_artist_top_tracks_memoized(artist_id())
      output$top_tracks_list <- renderUI({
        if (is.null(top_tracks) || nrow(top_tracks) == 0) {
          return(tags$p("No top tracks found."))
        }
        # Display the top 5 tracks with Spotify embed
        tagList(
          lapply(seq_len(min(5, nrow(top_tracks))), function(i) {
            track_id <- top_tracks$id[i]
            tags$iframe(
              style = "border-radius:12px",
              src = paste0(
                "https://open.spotify.com/embed/track/",
                track_id, "?utm_source=generator&theme=0"
              ),
              width = "100%",
              height = "80",
              frameBorder = "0",
              allowfullscreen = "",
              allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
              loading = "lazy"
            )
          })
        )
      })
    })
  })
}
