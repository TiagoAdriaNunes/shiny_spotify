# app/view/artist_profile.R
box::use(
  shiny[
    fluidPage, titlePanel, sidebarLayout, sidebarPanel, mainPanel,
    textOutput, moduleServer, NS, renderText, renderUI, uiOutput,
    observeEvent, req, tags  # tagList removed
  ],
  htmltools[tagList],  # tagList imported from htmltools
  spotifyr[get_artist, get_related_artists],
  memoise[memoise]
)

# Memoize the get_artist and get_related_artists functions to enable caching
get_artist_memo <- memoise(get_artist)
get_related_artists_memo <- memoise(get_related_artists)

# UI function for the artist profile
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Artist Profile"),
    uiOutput(ns("profile_content"))  # Dynamic output for profile content
  )
}

# Server function for the artist profile
#' @export
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use session to define ns within the server
    
    observeEvent(artist_id(), {  # Trigger this whenever the artist ID changes
      req(artist_id())  # Ensure artist_id is not empty
      
      # Use memoized function to get artist data
      artist_info <- get_artist_memo(artist_id())
      
      # Use memoized function to get related artists
      related_artists <- get_related_artists_memo(artist_id())
      
      # Dynamically render the UI for the artist profile
      output$profile_content <- renderUI({
        if (is.null(artist_info)) {
          return(NULL)  # Don't display anything if no artist is selected
        }
        sidebarLayout(
          sidebarPanel(
            textOutput(ns("artist_name")),  # Artist's name
            textOutput(ns("artist_genres")),  # Artist's genres
            textOutput(ns("artist_popularity")),  # Artist's popularity
            uiOutput(ns("related_artists"))  # Output for related artists
          ),
          mainPanel(
            uiOutput(ns("artist_image"))  # Render image in a dynamic output
          )
        )
      })
      
      # Render artist's image dynamically (only the first image)
      output$artist_image <- renderUI({
        tags$img(src = artist_info$images$url[1], width = "300px", height = "300px")  # Render only the first image
      })
      
      # Render artist's name
      output$artist_name <- renderText({
        paste("Name: ", artist_info$name)
      })
      
      # Render artist's popularity
      output$artist_popularity <- renderText({
        paste("Popularity: ", artist_info$popularity)
      })
      
      # Render artist's genres
      output$artist_genres <- renderText({
        paste("Genres: ", paste(artist_info$genres, collapse = ", "))
      })

      
      # Render related artists (first 5 related artists)
      output$related_artists <- renderUI({
        if (is.null(related_artists) || nrow(related_artists) == 0) {
          return(tags$p("No related artists found."))
        }
        
        # Show the first 5 related artists
        tagList(  # This is where tagList is used
          tags$h3("Related Artists:"),
          tags$ul(
            lapply(1:min(5, nrow(related_artists)), function(i) {
              tags$li(related_artists$name[i])
            })
          )
        )
      })
    })
  })
}
