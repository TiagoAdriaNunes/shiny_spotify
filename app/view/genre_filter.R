box::use(
  apexcharter[
    apex,
    apexchartOutput,
    renderApexchart,
    aes,
    ax_chart,
    ax_colors,
    ax_grid,
    ax_title,
    ax_tooltip,
    ax_xaxis,
    ax_yaxis
  ],
  checkmate[
    test_data_frame,
    test_numeric
  ],
  dplyr[
    arrange,
    desc,
    select
  ],
  htmltools[tags],
  htmlwidgets[JS],
  memoise[memoise],
  reactable[
    reactableOutput,
    renderReactable,
    colDef,
    colFormat,
    reactable,
    reactableTheme
  ],
  shiny[
    actionButton,
    fluidPage,
    mainPanel,
    moduleServer,
    NS,
    observeEvent,
    renderText,
    req,
    selectizeInput,
    sidebarLayout,
    sidebarPanel,
    textOutput,
    titlePanel
  ],
)

box::use(
  app / logic / lastfm_genres[
    add_artist_stats,
    get_artist_stats,
    get_genre_artists,
    get_top_genres
  ],
)

# Memoized functions for caching API calls. Artist stats are cached per
# artist, so genres that share artists don't look them up twice.
get_top_genres_memo <- memoise(get_top_genres)
get_genre_artists_memo <- memoise(get_genre_artists)
get_artist_stats_memo <- memoise(get_artist_stats)

# Number of top artists shown per genre; each costs one Last.fm call for stats
genre_artist_limit <- 20

# Last.fm's most used tags, offered as genres. If Last.fm is down the list is
# empty, but typing a genre still works (create = TRUE).
genre_choices <- function() {
  genres <- tryCatch(
    get_top_genres_memo(limit = 100),
    error = function(e) {
      warning("Last.fm top genres fetch failed: ", conditionMessage(e), call. = FALSE)
      character(0)
    }
  )
  c("", genres)
}

# UI function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Find Artists by Genre"),
    sidebarLayout(
      sidebarPanel(
        # Choices are rendered into the page rather than sent with
        # updateSelectizeInput() from the server: an update sent while the
        # session starts can reach the browser before the input is bound,
        # and Shiny silently drops it, leaving the list empty
        selectizeInput(
          ns("genre"),
          "Select Genre",
          choices = genre_choices(),
          options = list(
            create = TRUE,
            placeholder = "Type or select a genre"
          )
        ),
        actionButton(ns("search"), "Search"),
        tags$p(tags$small(class = "text-muted", "Genres and artists from Last.fm"))
      ),
      mainPanel(
        apexchartOutput(ns("listeners_chart")),
        reactableOutput(ns("artist_table")),
        textOutput(ns("message"))
      )
    )
  )
}

# Server function
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    clear_results <- function(message) {
      output$artist_table <- renderReactable(NULL)
      output$listeners_chart <- renderApexchart(NULL)
      output$message <- renderText(message)
    }

    observeEvent(input$search, {
      req(input$genre)
      search_failed <- FALSE
      artist_results <- tryCatch(
        get_genre_artists_memo(genre = input$genre, limit = genre_artist_limit),
        error = function(e) {
          warning("Last.fm genre search failed: ", conditionMessage(e), call. = FALSE)
          search_failed <<- TRUE
          NULL
        }
      )
      if (search_failed) {
        clear_results("Genre search unavailable. Please try again later.")
        return()
      }
      if (!test_data_frame(artist_results, min.rows = 1)) {
        clear_results(paste0("No artists found for the genre '", input$genre, "'. Please try a different genre."))
        return()
      }
      output$message <- renderText("")
      artist_results <- add_artist_stats(artist_results, get_stats = get_artist_stats_memo)
      output$artist_table <- renderReactable({
        reactable(
          artist_results |> select(rank, name, listeners, playcount, url),
          columns = list(
            rank = colDef(name = "Rank", width = 70),
            name = colDef(
              name = "Artist",
              cell = function(value, index) {
                tags$a(href = artist_results$url[index], target = "_blank", value)
              }
            ),
            listeners = colDef(
              name = "Listeners",
              format = colFormat(separators = TRUE, locales = "en-US")
            ),
            playcount = colDef(
              name = "Plays",
              format = colFormat(separators = TRUE, locales = "en-US")
            ),
            url = colDef(show = FALSE)
          ),
          theme = reactableTheme(
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
          pageSizeOptions = c(10, 20)
        )
      })
      output$listeners_chart <- renderApexchart({
        # Nothing to plot if every artist's stats lookup failed
        if (!test_numeric(artist_results$listeners, all.missing = FALSE)) {
          return(NULL)
        }
        apex(
          data = artist_results |> arrange(desc(listeners)),
          type = "bar",
          mapping = aes(x = name, y = listeners)
        ) |>
          ax_title(text = paste0("Top artists tagged '", input$genre, "' by Last.fm listeners")) |>
          ax_xaxis(
            title = list(text = "Artist"),
            labels = list(style = list(colors = "#E0E0E0")),
            axisBorder = list(show = TRUE, color = "#444444"),
            axisTicks = list(show = TRUE, color = "#444444")
          ) |>
          ax_yaxis(
            title = list(text = "Listeners"),
            labels = list(
              style = list(colors = "#E0E0E0"),
              formatter = JS("function(value) { return value.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','); }")
            ),
            axisBorder = list(show = TRUE, color = "#444444"),
            axisTicks = list(show = TRUE, color = "#444444"),
            tickAmount = 10
          ) |>
          ax_chart(
            background = "#2B2B2B"
          ) |>
          ax_colors("#1F77B4") |>
          ax_grid(
            borderColor = "#444444"
          ) |>
          ax_tooltip(
            theme = "dark"
          )
      })
    })
  })
}
