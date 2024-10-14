# genre_filter.R
box::use(
  dplyr[`%>%`, select],
  memoise[memoise],
  shiny[...], #nolint
  spotifyr[get_genre_artists],
)

box::use(
  app/config/genres,
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
            placeholder = "Type or select a genre"
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
          tags$p("No artists found for the genre '",
                 tags$b(input$genre),
                 "'. Please try a different genre.")
        })
      } else {
<<<<<<< Updated upstream
        # Clear any previous messages
        output$message <- renderUI({
          NULL
=======
        output$message <- shiny::renderText(
                                            { "" })
        artist_results <- artist_results %>%
          dplyr::mutate(genres = sapply(genres, function(g) paste(g, collapse = ", "))) %>%
          dplyr::arrange(desc(followers.total), desc(popularity))
        # Filter to top 20 artists by followers
        top_20_artists <- artist_results %>%
          dplyr::arrange(desc(followers.total)) %>%
          dplyr::slice(1:20)
        output$artist_table <- reactable::renderReactable({
          reactable::reactable(
            artist_results %>% dplyr::select(name, popularity, followers.total, genres),
            columns = list(
              name = reactable::colDef(name = "Name"),
              popularity = reactable::colDef(name = "Popularity"),
              followers.total = reactable::colDef(
                name = "Followers",
                format = reactable::colFormat(separators = TRUE)
              ),
              genres = reactable::colDef(name = "Genres")
            ),
            theme = reactable::reactableTheme(
              backgroundColor = "#2B2B2B",
              color = "#E0E0E0",
              borderColor = "#444444",
              headerStyle = list(
                backgroundColor = "#1F1F1F",
                color = "#E0E0E0"
              ),
              tableBodyStyle = list(
                backgroundColor = "#2B2B2B"
              ),
              rowHighlightStyle = list(
                backgroundColor = "#3A3A3A"
              ),
              paginationStyle = list(
                backgroundColor = "#1F1F1F",
                color = "#E0E0E0"
              ),
              pageButtonHoverStyle = list(
                backgroundColor = "#3A3A3A"
              )
            ),
            pagination = TRUE,
            paginationType = "simple",
            defaultPageSize = 10,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 20, 50)
          )
>>>>>>> Stashed changes
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
