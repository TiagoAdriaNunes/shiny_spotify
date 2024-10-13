# related_artists.R

# Import necessary libraries and functions
box::use(
  htmltools[HTML],
  memoise[memoise],
  shiny[...], #nolint
  spotifyr[get_related_artists],
)

# Memoize the Spotify API function for caching
get_related_artists_memo <- memoise(get_related_artists)

# UI function for the related artists
#' @export
ui <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("related_artists"))
}

# Helper function to fetch related artists
fetch_related_artists <- function(artist_id) {
  tryCatch({
    get_related_artists_memo(artist_id)
  }, error = function(e) {
    NULL
  })
}

# Helper function to render related artists
render_related_artists <- function(ns, related_artists) {
  if (is.null(related_artists) ||
        !is.data.frame(related_artists) ||
        nrow(related_artists) == 0) {
    return(tags$p("No related artists found."))
  }
  tagList(
    tags$ul(lapply(seq_len(min(5, nrow(related_artists))), function(i) {
      tags$li(related_artists$name[i])
    }))
  )
}

# Server function for the related artists
#' @export
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # React to artist_id changes
    observeEvent(artist_id(), {
      req(artist_id())
      # Fetch related artists
      related_artists <- fetch_related_artists(artist_id())
      # Render related artists
      output$related_artists <- renderUI({
        render_related_artists(ns, related_artists)
      })
    })
  })
}
