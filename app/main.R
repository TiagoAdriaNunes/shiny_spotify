# main.R

box::use(
  bslib[...], #nolint,
  shiny[...], #nolint,
)

box::use(
  app/logic/auth, #nolint
  app/view/artist_profile,
  app/view/artist_search,
  app/view/artist_top_tracks,
  app/view/genre_filter,
  app/view/related_artists,
)

# Top-level UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    theme = bs_theme(bootswatch = "darkly"),
    navbarPage(
      title = "Spotify Search App",
      tabPanel("Artist Profile",
        layout_columns(
          card(card_header("Artist Search"), artist_search$ui(ns("artist_search"))),
          card(card_header("Artist Profile"), artist_profile$ui(ns("artist_profile"))),
          card(card_header("Top Tracks"), artist_top_tracks$ui(ns("artist_top_tracks"))),
          card(card_header("Related Artists"), related_artists$ui(ns("related_artists"))),
          col_widths = breakpoints(
            sm = c(6, 6, 6, 6),
            md = c(6, 6, 6, 6),
            lg = c(3, 3, 3, 3)
          )
        )
      ),
      tabPanel("Search by Genre", genre_filter$ui(ns("genre_filter")))
    ),
    tags$p(id = ns("message"), "Spotify Search App!")
  )
}

# Top-level server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initialize artist_id as a reactive value
    selected_artist_id <- reactiveVal(NULL)
    # Initialize name_artist as a reactive value
    selected_artist_name <- reactiveVal(NULL)
    # Call artist search server and pass the reactive selected_artist_id
    artist_search$server("artist_search", selected_artist_id, selected_artist_name)
    # Call artist profile server and pass the reactive selected_artist_id
    artist_profile$server("artist_profile", selected_artist_id)
    # Call artist top tracks server and pass the reactive selected_artist_id
    artist_top_tracks$server("artist_top_tracks", selected_artist_id)
    # Call related artists server and pass the reactive selected_artist_id
    related_artists$server("related_artists", selected_artist_id, selected_artist_name)
    # Call genre filter server logic
    genre_filter$server("genre_filter")
    # Define output$message
    output$message <- renderText({
      "Spotify Search App!"
    })
  })
}
