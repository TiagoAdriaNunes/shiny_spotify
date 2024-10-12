# app/view/genre_filter.R
box::use(
  shiny[
    fluidPage, titlePanel, sidebarLayout, sidebarPanel, mainPanel, selectizeInput,
    textInput, numericInput, actionButton, renderTable, tableOutput,
    moduleServer, NS, req, observeEvent, renderUI, uiOutput, tags, updateSelectizeInput, observe  # Added updateSelectizeInput
  ],
  spotifyr[get_genre_artists],
  memoise[memoise],
  dplyr[select, `%>%`],
  app/config/genres
)

# Memoize the Spotify API function to enable caching
get_genre_artists_memo <- memoise(get_genre_artists)

# UI function for genre filter
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Find Artists by Genre"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          ns("genre"), "Select Genre",
          choices = NULL,  # Allow any genre to be input
          options = list(
            create = TRUE,  # Allows user to input new genres
            placeholder = 'Type or select a genre'
          )
        ),
        textInput(ns("market"), "Enter Country Code (Optional)", placeholder = "e.g., US"),
        numericInput(ns("limit"), "Limit of Results", value = 10, min = 1, max = 50),
        actionButton(ns("search"), "Search Artists")
      ),
      mainPanel(
        tableOutput(ns("artist_table")),
        uiOutput(ns("message"))  # Output area for informative messages
      )
    )
  )
}

# Server function for genre filter
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use session to define ns within the server

    # Update selectizeInput choices on the client side
    observe({
      updateSelectizeInput(session, "genre",
        choices = genres$genres_list,
        server = FALSE  # Load choices client-side to allow create = TRUE to work
      )
    })

    observeEvent(input$search, {
      req(input$genre)  # Ensure a genre is inputted

      # Attempt to retrieve artists for the inputted genre
      artist_results <- tryCatch({
        if (input$market == "") {
          get_genre_artists_memo(
            genre = input$genre,
            limit = input$limit
          )
        } else {
          get_genre_artists_memo(
            genre = input$genre,
            market = input$market,
            limit = input$limit
          )
        }
      }, error = function(e) {
        # Handle API errors
        output$message <- renderUI({
          tags$p("An error occurred: ", e$message)
        })
        NULL
      })

      # Check if any artists were found
      if (is.null(artist_results) || nrow(artist_results) == 0) {
        output$artist_table <- renderTable({
          NULL  # Clear the table output
        })
        output$message <- renderUI({
          tags$p("No artists found for the genre '", tags$b(input$genre), "'. Please try a different genre.")
        })
      } else {
        # Clear any previous messages
        output$message <- renderUI({
          NULL
        })
        # Display the artist results
        output$artist_table <- renderTable({
          artist_results %>%
            select(name, popularity, followers.total)  # Show relevant columns
        })
      }
    })
  })
}
