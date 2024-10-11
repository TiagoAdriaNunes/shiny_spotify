# app/main.R
box::use(
  shiny[fluidPage, titlePanel, tabsetPanel, tabPanel, fluidRow, column, moduleServer, NS, reactiveVal],  # Import titlePanel and other shiny components
  app/view/artist_search,  # Import the artist search module
  app/view/artist_profile,  # Import the artist profile module
  app/view/genre_filter     # Import the genre filter module
)

# Top-level UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Spotify Search App"),  # App title
    tabsetPanel(
      tabPanel("Search by Artist", 
               fluidRow(
                 column(6, artist_search$ui(ns("artist_search"))),  # Artist search on the left
                 column(6, artist_profile$ui(ns("artist_profile")))  # Artist profile on the right
               )
      ),
      tabPanel("Search by Genre", genre_filter$ui(ns("genre_filter")))  # Genre filter tab
    )
  )
}

# Top-level server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize artist_id as a reactive value
    selected_artist_id <- reactiveVal(NULL)  # Starts as NULL
    
    # Call artist search server and pass the reactive selected_artist_id
    artist_search$server("artist_search", selected_artist_id)
    
    # Call artist profile server and pass the reactive selected_artist_id
    artist_profile$server("artist_profile", selected_artist_id)  # Pass the reactive artist ID
    
    # Call genre filter server logic
    genre_filter$server("genre_filter")
  })
}

