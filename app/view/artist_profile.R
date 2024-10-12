# artist_profile.R

# Import necessary libraries and functions
box::use(
  htmltools[tagList],
  memoise[memoise],
  shiny[...], #nolint
  spotifyr[get_artist, get_related_artists],
)

# Memoize the Spotify API functions for caching
get_artist_memo <- memoise(get_artist)
get_related_artists_memo <- memoise(get_related_artists)

# UI function for the artist profile
#' @export
ui <- function(id) {
  ns <- NS(id)
  # Note: No need to include uiOutput for artist_image here
  fluidPage(
    titlePanel("Artist Profile"),
    uiOutput(ns("profile_content"))  # Dynamic output for profile content
  )
}

# Helper function to fetch artist data
fetch_artist_data <- function(artist_id) {
  tryCatch({
    get_artist_memo(artist_id)
  }, error = function(e) {
    NULL
  })
}

# Helper function to fetch related artists
fetch_related_artists <- function(artist_id) {
  tryCatch({
    get_related_artists_memo(artist_id)
  }, error = function(e) {
    NULL
  })
}

# Helper function to render artist information
render_artist_info <- function(ns, artist_info) {
  fluidPage(fluidRow(
    # Artist information
    column(
      6,
      tags$h3("Artist Profile"),
      tags$p(textOutput(ns("artist_name"))),
      tags$p(textOutput(ns("artist_genres"))),
      tags$p(textOutput(ns("artist_popularity")))
    ),
    # Related artists and image
    column(
      6,
      uiOutput(ns("related_artists")),
      uiOutput(ns("artist_image"))
    )
  ))
}

# Helper function to render related artists
render_related_artists <- function(ns, related_artists) {
  if (is.null(related_artists) ||
        !is.data.frame(related_artists) ||
        nrow(related_artists) == 0) {
    return(tags$p("No related artists found."))
  }
  tagList(
    tags$h3("Related Artists:"),
    tags$ul(lapply(seq_len(min(5, nrow(related_artists))), function(i) {
      tags$li(related_artists$name[i])
    }))
  )
}

# Server function for the artist profile
#' @export
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # React to artist_id changes
    observeEvent(artist_id(), {
      req(artist_id())
      # Fetch artist data
      artist_info <- fetch_artist_data(artist_id())
      # Fetch related artists
      related_artists <- fetch_related_artists(artist_id())
      # Render the UI for the artist profile
      output$profile_content <- renderUI({
        if (is.null(artist_info) || !is.list(artist_info)) {
          return(tags$p("No artist information available."))
        }
        render_artist_info(ns, artist_info)
      })
      # Render artist's image dynamically (only the first image)
      output$artist_image <- renderUI({
        tags$img(src = artist_info$images$url[2])
      })
      # Render artist's name
      output$artist_name <- renderText({
        if (!is.null(artist_info$name)) {
          paste("Name:", artist_info$name)
        } else {
          "Name not available."
        }
      })
      # Render artist's popularity
      output$artist_popularity <- renderText({
        if (!is.null(artist_info$popularity)) {
          paste("Popularity:", artist_info$popularity)
        } else {
          "Popularity not available."
        }
      })
      # Render artist's genres
      output$artist_genres <- renderText({
        if (!is.null(artist_info$genres) && length(artist_info$genres) > 0) {
          paste("Genres:", paste(artist_info$genres, collapse = ", "))
        } else {
          "Genres not available."
        }
      })
      # Render related artists
      output$related_artists <- renderUI({
        render_related_artists(ns, related_artists)
      })
    })
  })
}
