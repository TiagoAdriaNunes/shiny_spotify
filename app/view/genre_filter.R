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
  checkmate[test_data_frame],
  dplyr[
    arrange,
    desc,
    mutate,
    select,
    slice_head
  ],
  htmlwidgets[JS],
  memoise[memoise],
  purrr[map_chr],
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
  app / config / genres[genres_list],
  app / logic / spotify_api[get_genre_artists],
)

# Memoized function for caching API calls
get_genre_artists_memo <- memoise(get_genre_artists)

# UI function
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Find Artists by Genre"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          ns("genre"),
          "Select Genre",
          choices = c("", genres_list),
          selected = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select a genre"
          )
        ),
        actionButton(ns("search"), "Search")
      ),
      mainPanel(
        apexchartOutput(ns("followers_chart")),
        reactableOutput(ns("artist_table")),
        textOutput(ns("message"))
      )
    )
  )
}

# Server function
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$search, {
      req(input$genre)
      artist_results <- tryCatch(
        {
          # Spotify's /v1/search only accepts limit 0-10 (see
          # app/logic/spotify_api.R)
          get_genre_artists_memo(genre = input$genre, limit = 10)
        },
        error = function(e) {
          output$message <- renderText({
            paste("An error occurred:", e$message)
          })
          NULL
        }
      )
      if (!test_data_frame(artist_results, min.rows = 1)) {
        output$artist_table <- renderReactable({
          NULL
        })
        output$followers_chart <- renderApexchart({
          NULL
        })
        output$message <- renderText({
          paste("No artists found for the genre '", input$genre, "'. Please try a different genre.")
        })
      } else {
        output$message <- renderText({
          ""
        })
        # Spotify omits genres/followers/popularity for some access tiers
        # (see app/logic/spotify_api.R); fill in safe defaults so the
        # pipeline below doesn't error when a field is missing entirely.
        if (!"genres" %in% names(artist_results)) {
          artist_results$genres <- vector("list", nrow(artist_results))
        }
        if (!"followers.total" %in% names(artist_results)) {
          artist_results$followers.total <- NA_real_
        }
        if (!"popularity" %in% names(artist_results)) {
          artist_results$popularity <- NA_real_
        }
        artist_results <- artist_results |>
          mutate(genres = map_chr(genres, \(g) paste(g, collapse = ", "))) |>
          arrange(desc(followers.total), desc(popularity))
        # Filter to top 20 artists by followers
        top_20_artists <- artist_results |>
          slice_head(n = 20)
        output$artist_table <- renderReactable({
          reactable(
            artist_results |> select(name, popularity, followers.total, genres),
            columns = list(
              name = colDef(name = "Name"),
              popularity = colDef(name = "Popularity"),
              followers.total = colDef(
                name = "Followers",
                format = colFormat(separators = TRUE, locales = "en-US")
              ),
              genres = colDef(name = "Genres")
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
            pageSizeOptions = c(10, 20, 50)
          )
        })
        output$followers_chart <- renderApexchart({
          apex(
            data = top_20_artists,
            type = "bar",
            mapping = aes(x = name, y = followers.total)
          ) |>
            ax_title(text = "Top 20 Artists by Total Followers in this genre") |>
            ax_xaxis(
              title = list(text = "Artist"),
              labels = list(style = list(colors = "#E0E0E0")),
              axisBorder = list(show = TRUE, color = "#444444"),
              axisTicks = list(show = TRUE, color = "#444444")
            ) |>
            ax_yaxis(
              title = list(text = "Total Followers"),
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
      }
    })
  })
}
