box::use(
  checkmate[
    test_data_frame,
    test_string
  ],
  htmltools[tagList],
  memoise[memoise],
  purrr[map2],
  shiny[
    htmlOutput,
    moduleServer,
    NS,
    renderUI,
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
    # Fetched inside the render function so the card is marked as
    # recalculating, and shows a spinner, while the request runs
    output$top_tracks_list <- renderUI({
      name <- artist_name()
      if (!test_string(name, min.chars = 1)) {
        # No artist selected yet
        return(tags$p("Please select an artist to see their top tracks."))
      }
      # Fetch top tracks for the artist via search (get_artist_top_tracks's
      # underlying endpoint is deprecated -- see app/logic/spotify_api.R)
      top_tracks <- tryCatch(
        get_artist_top_tracks_memoized(name),
        error = function(e) {
          warning("Spotify top tracks fetch failed: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (!test_data_frame(top_tracks, min.rows = 1)) {
        return(tags$p("No top tracks found."))
      }
      # Display the top 5 tracks with Spotify embed
      shown <- head(top_tracks, 5)
      tagList(
        map2(shown$id, shown$name, function(track_id, track_name) {
          # Each player loads on its own. The wrapper is a placeholder that
          # holds its slot, and the stylesheet fades the player in once its
          # onload marks it loaded, instead of it flashing white and popping in
          tags$div(
            class = "track-embed",
            tags$iframe(
              src = paste0(
                "https://open.spotify.com/embed/track/",
                track_id,
                "?utm_source=generator&theme=0"
              ),
              title = paste("Spotify player:", track_name),
              width = "100%",
              height = "80",
              frameBorder = "0",
              allowfullscreen = "",
              allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
              loading = "lazy",
              onload = "this.classList.add('is-loaded')"
            )
          )
        })
      )
    })
  })
}
