# main.R

box::use(
  shiny[...],#nolint
)

box::use(
  app/view/artist_profile,
  app/view/artist_search,
  app/view/artist_top_tracks,
  app/view/genre_filter,
)

# Top-level UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Spotify Search App"),
    tabsetPanel(
      tabPanel("Search by Artist", fluidRow(
        column(6, artist_search$ui(ns("artist_search"))),
        column(6,
          # Use artist_profile UI which includes the image
          # Include top tracks below the artist profile
          artist_profile$ui(ns("artist_profile")),
          artist_top_tracks$ui(ns("artist_top_tracks"))
        )
      )),
      tabPanel("Search by Genre", genre_filter$ui(ns("genre_filter")))
    ),
    uiOutput(ns("message"))  # Add a UI output for the message
  )
}

# Top-level server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initialize artist_id as a reactive value
    selected_artist_id <- reactiveVal(NULL)
    # Call artist search server and pass the reactive selected_artist_id
    artist_search$server("artist_search", selected_artist_id)
    # Call artist profile server and pass the reactive selected_artist_id
    artist_profile$server("artist_profile", selected_artist_id)
    # Call artist top tracks server and pass the reactive selected_artist_id
    artist_top_tracks$server("artist_top_tracks", selected_artist_id)
    # Call genre filter server logic
    genre_filter$server("genre_filter")
    # Define the message output
    output$message <- renderUI({
      tags$p("Spotify Search App!")
    })
  })
}
