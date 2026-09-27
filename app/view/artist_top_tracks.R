box::use(
  checkmate[
    test_data_frame,
    test_string
  ],
  htmltools[tagList],
  memoise[memoise],
  purrr[map],
  shiny[
    htmlOutput,
    moduleServer,
    NS,
    observeEvent,
    renderUI,
    req,
    tags
  ],
  utils[head],
)
box::use(
  app / logic / spotify_api[get_artist_top_tracks],
)

# Memoize the get_artist_top_tracks function to cache the results
get_artist_top_tracks_memoized <- memoise(get_artist_top_tracks)

# UI function for the artist's top tracks
ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("top_tracks_list"))
  )
}

# Server function for the artist's top tracks
server <- function(id, artist_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Use session to define ns within the server
    # Observe changes in artist_name (reactive)
    observeEvent(artist_name(), {
      if (!test_string(artist_name(), min.chars = 1)) {
        # No artist selected yet
        output$top_tracks_list <- renderUI({
          tags$p("Please select an artist to see their top tracks.")
        })
        return()
      }
      # If artist_name is available, proceed with fetching top tracks
      req(artist_name()) # Ensure artist_name is not empty or invalid
      # Fetch top tracks for the artist via search (get_artist_top_tracks's
      # underlying endpoint is deprecated -- see app/logic/spotify_api.R)
      top_tracks <- tryCatch(
        get_artist_top_tracks_memoized(artist_name()),
        error = function(e) {
          warning("Spotify top tracks fetch failed: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      output$top_tracks_list <- renderUI({
        if (!test_data_frame(top_tracks, min.rows = 1)) {
          return(tags$p("No top tracks found."))
        }
        # Display the top 5 tracks with Spotify embed
        tagList(
          map(head(top_tracks$id, 5), function(track_id) {
            tags$iframe(
              style = "border-radius:12px",
              src = paste0(
                "https://open.spotify.com/embed/track/",
                track_id,
                "?utm_source=generator&theme=0"
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
