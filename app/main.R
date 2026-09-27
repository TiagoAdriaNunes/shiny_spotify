box::use(
  bslib[
    bs_theme,
    breakpoints,
    card,
    card_header,
    layout_columns,
    page_fillable
  ],
  shiny[
    busyIndicatorOptions,
    moduleServer,
    navbarPage,
    NS,
    reactiveVal,
    renderText,
    tabPanel,
    tags,
    useBusyIndicators
  ],
)

box::use(
  app / logic / auth,
  app / view / artist_profile,
  app / view / artist_search,
  app / view / artist_top_tracks,
  app / view / genre_filter,
  app / view / related_artists,
)

# Artist shown when the app opens, before anything is searched. The Spotify
# ID is used directly so startup doesn't need a search request.
default_artist <- list(id = "4tZwfgrHOc3mvqYlEYSvVi", name = "Daft Punk")

# Top-level UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    theme = bs_theme(
      bootswatch = "darkly",
      navbar_bg = "#1DB954",
      navbar_light_color = "white"
    ),
    # Show a spinner on each card while its data loads, and a thin bar at the
    # top of the page whenever the server is busy
    useBusyIndicators(),
    busyIndicatorOptions(
      spinner_color = "#1DB954",
      pulse_background = "linear-gradient(45deg, #1DB954, #1ED760)"
    ),
    tags$div(
      "Spotify API was changed, the app is being adjusted.",
      style = paste(
        "background-color: #b91d1d; color: white; padding: 8px 16px;",
        "text-align: center; font-weight: bold;"
      )
    ),
    navbarPage(
      title = "Spotify Search App",
      inverse = TRUE,
      windowTitle = "Spotify Search App",
      tabPanel(
        "Artist Profile",
        card(artist_search$ui(ns("artist_search"))),
        layout_columns(
          card(card_header("Artist Profile"), artist_profile$ui(ns("artist_profile"))),
          card(card_header("Top Tracks"), artist_top_tracks$ui(ns("artist_top_tracks"))),
          card(card_header("Related Artists powered by Last.fm"), related_artists$ui(ns("related_artists"))),
          # The related artists network needs width, so it gets its own row
          # until the screen is wide enough for three columns
          col_widths = breakpoints(
            sm = c(6, 6, 12),
            md = c(6, 6, 12),
            lg = c(4, 4, 4)
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
    # Start with the default artist selected so the page isn't empty
    selected_artist_id <- reactiveVal(default_artist$id)
    selected_artist_name <- reactiveVal(default_artist$name)
    # Call artist search server and pass the reactive selected_artist_id
    artist_search$server("artist_search", selected_artist_id, selected_artist_name)
    # Call artist profile server and pass the reactive selected_artist_id
    artist_profile$server("artist_profile", selected_artist_id)
    # Call artist top tracks server and pass the reactive selected_artist_name
    artist_top_tracks$server("artist_top_tracks", selected_artist_name)
    # Call related artists server and pass only the artist name
    related_artists$server("related_artists", selected_artist_name)
    # Call genre filter server logic
    genre_filter$server("genre_filter")
    # Define output$message
    output$message <- renderText({
      "Spotify Search App!"
    })
  })
}
