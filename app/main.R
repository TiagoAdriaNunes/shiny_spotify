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
    updateNavbarPage,
    useBusyIndicators
  ],
)

box::use(
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
    navbarPage(
      # An id so the server can switch tabs, e.g. to open an artist's profile
      # from the genre results
      id = ns("tabs"),
      title = "Spotify Search App",
      inverse = TRUE,
      windowTitle = "Spotify Search App",
      tabPanel(
        "Artist Profile",
        card(artist_search$ui(ns("artist_search"))),
        # .result-card reserves each card's loaded height (see main.scss), so
        # the page doesn't jump as the cards fill in on the first load
        layout_columns(
          card(
            card_header("Artist Profile"),
            artist_profile$ui(ns("artist_profile")),
            class = "result-card"
          ),
          card(
            card_header("Top Tracks"),
            artist_top_tracks$ui(ns("artist_top_tracks")),
            class = "result-card"
          ),
          card(
            card_header("Related Artists powered by Last.fm"),
            related_artists$ui(ns("related_artists")),
            class = "result-card"
          ),
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
    # Call artist search server and pass the reactive selected_artist_id. It
    # returns open_artist(name), which runs the same search from elsewhere.
    open_artist <- artist_search$server("artist_search", selected_artist_id, selected_artist_name)
    # Call artist top tracks server and pass the reactive selected_artist_name
    artist_top_tracks$server("artist_top_tracks", selected_artist_name, selected_artist_id)
    # Opens an artist's profile from anywhere in the app (the genre results,
    # the related artists network): searches for them and, when found, shows
    # the profile tab. Returns whether they were found.
    open_artist_profile <- function(name) {
      found <- open_artist(name)
      if (found) {
        updateNavbarPage(session, "tabs", selected = "Artist Profile")
      }
      found
    }
    # Call related artists server; selecting a node offers to open its profile
    related_artists$server("related_artists", selected_artist_name, open_artist = open_artist_profile)
    # Clicking an artist in the genre results opens their profile
    search_genre <- genre_filter$server("genre_filter", open_artist = open_artist_profile)
    open_genre <- function(genre) {
      updateNavbarPage(session, "tabs", selected = "Search by Genre")
      search_genre(genre)
    }
    artist_profile$server("artist_profile", selected_artist_id, open_genre = open_genre)
    # Define output$message
    output$message <- renderText({
      "Spotify Search App!"
    })
  })
}
