# app/view/artist_search.R
box::use(
  shiny[fluidPage, titlePanel, sidebarLayout, sidebarPanel, mainPanel, textInput, actionButton, renderText, textOutput, moduleServer, NS, req, observeEvent],
  spotifyr[search_spotify],
  memoise[memoise]
)

# Memoize the Spotify API functions to enable caching
search_spotify_memo <- memoise(search_spotify)

# UI function for the module
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Spotify Artist Search"),  # Title of the page
    sidebarLayout(
      sidebarPanel(
        textInput(ns("artist_name"), "Enter artist name", value = "Kendrick Lamar"),  # Input for artist name
        actionButton(ns("search"), "Search")  # Button to trigger search
      ),
      mainPanel(
        textOutput(ns("artist_info"))  # Output to display artist information or error message
      )
    )
  )
}

# Server function for the module
#' @export
server <- function(id, selected_artist_id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$search, {
      req(input$artist_name)  # Ensure artist_name input is not empty
      
      # Use the memoized version of search_spotify to cache the results
      artist_result <- search_spotify_memo(input$artist_name, type = "artist")
      
      if (nrow(artist_result) > 0) {
        artist_id <- artist_result$id[1]  # Get the first result's artist ID
        artist_name <- artist_result$name[1]  # Get the artist name
        
        # Store the artist ID in the reactive value
        selected_artist_id(artist_id)
        
        # Display artist's name in the output
        output$artist_info <- renderText({
          paste("Found artist:", artist_name)
        })
      } else {
        output$artist_info <- renderText("Artist not found.")
      }
    })
  })
}
