box::use(
  checkmate[
    assert_data_frame,
    test_data_frame,
    test_names,
    test_string
  ],
  dplyr[
    distinct,
    mutate,
    select
  ],
  htmltools[tagList],
  memoise[memoise],
  purrr[
    map_chr,
    pmap
  ],
  shiny[
    htmlOutput,
    moduleServer,
    NS,
    renderUI,
    tags
  ],
  utils[head, tail],
)
box::use(
  app / logic / spotify_api[get_artist_top_tracks],
)

# Memoize the get_artist_top_tracks function to cache the results
get_artist_top_tracks_memoized <- memoise(get_artist_top_tracks)

# UI function for the artist's top tracks
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("top_tracks_list"))
  )
}

#' Drop repeats of the same song. Search returns each release separately, so
#' e.g. "Go", "Go" and "Go - Radio Edit" can all come back; the first (most
#' relevant) one is kept.
#' @export
unique_tracks <- function(tracks) {
  assert_data_frame(tracks)
  tracks |>
    mutate(
      # Title without a " - Radio Edit" / " (Remastered)" style suffix
      song = tolower(trimws(sub("\\s*(-\\s.*|\\(.*\\))$", "", name)))
    ) |>
    distinct(song, .keep_all = TRUE) |>
    select(-song)
}

# Each track's artist names, comma separated ("" if Spotify left them out)
track_artists <- function(tracks) {
  if (!test_names(names(tracks), must.include = "artists")) {
    return(rep("", nrow(tracks)))
  }
  map_chr(tracks$artists, \(artists) paste(artists$name, collapse = ", "))
}

# Each track's smallest album cover (Spotify lists them largest first, down to
# 64px), or "" if there's none
track_covers <- function(tracks) {
  if (!test_names(names(tracks), must.include = "album.images")) {
    return(rep("", nrow(tracks)))
  }
  map_chr(
    tracks$album.images,
    \(images) if (test_data_frame(images, min.rows = 1)) tail(images$url, 1) else ""
  )
}

# The track's details shown straight away, laid out like Spotify's compact
# player, while the player itself loads over it
track_placeholder <- function(name, artists, cover) {
  tags$div(
    class = "track-embed-placeholder",
    `aria-hidden` = "true",
    if (test_string(cover, min.chars = 1)) tags$img(src = cover, alt = "", width = 64, height = 64),
    tags$div(
      class = "track-embed-text",
      tags$div(class = "track-embed-title", name),
      tags$div(class = "track-embed-artists", artists)
    )
  )
}

# Server function for the artist's top tracks
#' @export
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
      shown <- head(unique_tracks(top_tracks), 5)
      # Artist names and the smallest (64px) album cover, for the placeholders
      artists <- track_artists(shown)
      covers <- track_covers(shown)
      tagList(
        pmap(list(shown$id, shown$name, artists, covers), function(track_id, track_name, track_artists, cover) {
          # Each player loads on its own. Its placeholder shows the track
          # straight away, and the stylesheet fades the player in over it once
          # its onload marks it loaded, instead of it flashing and popping in
          tags$div(
            class = "track-embed",
            track_placeholder(track_name, track_artists, cover),
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
