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
    ax_yaxis,
    set_input_click
  ],
  bsicons[bs_icon],
  bslib[input_task_button],
  checkmate[
    test_data_frame,
    test_function,
    test_numeric,
    test_string
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
    eventReactive,
    fluidPage,
    mainPanel,
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
    renderText,
    req,
    selectizeInput,
    sidebarLayout,
    sidebarPanel,
    textOutput,
    titlePanel
  ],
  stringr[str_glue],
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

# An artist in the results table: their name opens their profile in the app,
# and the icon next to it opens their Last.fm page. The name is a button, and
# app/js/index.js sends its data-artist to the Shiny input in data-input-id
# when clicked.
genre_artist_cell <- function(name, lastfm_url, input_id, clickable = TRUE) {
  lastfm_link <- tags$a(
    href = lastfm_url,
    target = "_blank",
    rel = "noopener",
    class = "lastfm-link",
    title = "Open on Last.fm",
    `aria-label` = as.character(str_glue("Open {name} on Last.fm")),
    bs_icon("box-arrow-up-right")
  )
  if (!clickable) {
    return(tags$span(class = "genre-artist", name, lastfm_link))
  }
  tags$span(
    class = "genre-artist",
    tags$button(
      type = "button",
      class = "genre-artist-link",
      title = as.character(str_glue("Open {name}'s profile")),
      `data-input-id` = input_id,
      `data-artist` = name,
      name
    ),
    lastfm_link
  )
}

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
        # No busy icon: the table and chart show the loading spinner, so the
        # button just says "Searching..." and is disabled
        input_task_button(ns("search"), "Search", label_busy = "Searching...", icon_busy = NULL, type = "default"),
        tags$p(tags$small(class = "text-muted", "Genres and artists from Last.fm"))
      ),
      mainPanel(
        # The chart output is a fixed 400px already; the table's height is
        # auto, so .genre-table reserves its space (see main.scss) to keep
        # the page from jumping
        tags$div(
          class = "genre-chart",
          apexchartOutput(ns("listeners_chart"))
        ),
        tags$div(
          class = "genre-table",
          reactableOutput(ns("artist_table"))
        ),
        textOutput(ns("message"))
      )
    )
  )
}

# Server function
#' @param open_artist Function taking an artist name that opens their profile
#'   and returns TRUE, or FALSE when Spotify has no match (see
#'   artist_search's server). NULL makes the artists not clickable.
server <- function(id, open_artist = NULL) {
  moduleServer(id, function(input, output, session) {
    # Runs a search when the button is clicked. The table, chart and message
    # all read this, so while it runs they're marked as recalculating and show
    # a spinner. Returns the searched genre, a status ("ok", "empty" or
    # "failed") and, when "ok", the artists with their Last.fm stats.
    search_results <- eventReactive(input$search, {
      req(input$genre)
      genre <- input$genre
      search_failed <- FALSE
      artists <- tryCatch(
        get_genre_artists_memo(genre = genre, limit = genre_artist_limit),
        error = function(e) {
          warning("Last.fm genre search failed: ", conditionMessage(e), call. = FALSE)
          search_failed <<- TRUE
          NULL
        }
      )
      if (search_failed) {
        return(list(genre = genre, status = "failed"))
      }
      if (!test_data_frame(artists, min.rows = 1)) {
        return(list(genre = genre, status = "empty"))
      }
      artists <- add_artist_stats(artists, get_stats = get_artist_stats_memo)
      list(genre = genre, status = "ok", artists = artists)
    })

    # Clicking an artist, by name in the table or by bar in the chart, opens
    # their profile. When Spotify has no match the user stays here and sees
    # why; a new genre search clears that message.
    open_error <- reactiveVal("")
    observeEvent(input$search, open_error(""))
    open_clicked_artist <- function(name) {
      req(test_function(open_artist), test_string(name, min.chars = 1))
      open_error("")
      if (!open_artist(name)) {
        open_error(as.character(str_glue("'{name}' wasn't found on Spotify.")))
      }
    }
    observeEvent(input$artist_clicked, open_clicked_artist(input$artist_clicked))
    # The chart sends the clicked bar's category (the artist name), and NULL
    # when the bar is clicked again to deselect it, which observeEvent ignores
    observeEvent(input$chart_click, open_clicked_artist(unlist(input$chart_click)[1]))

    output$message <- renderText({
      if (test_string(open_error(), min.chars = 1)) {
        return(open_error())
      }
      results <- search_results()
      switch(
        results$status,
        failed = "Genre search unavailable. Please try again later.",
        empty = paste0("No artists found for the genre '", results$genre, "'. Please try a different genre."),
        ""
      )
    })

    output$artist_table <- renderReactable({
      results <- search_results()
      if (results$status != "ok") {
        return(NULL)
      }
      artist_results <- results$artists
      reactable(
        artist_results |> select(rank, name, listeners, playcount, url),
        columns = list(
          rank = colDef(name = "Rank", width = 70),
          name = colDef(
            name = "Artist",
            # The cell is rendered as an HTML string: reactable turns R tags
            # into its own elements and would show the icon's raw SVG
            # (bs_icon() returns HTML) as text
            html = TRUE,
            cell = function(value, index) {
              as.character(genre_artist_cell(
                value,
                artist_results$url[index],
                input_id = session$ns("artist_clicked"),
                clickable = test_function(open_artist)
              ))
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
      results <- search_results()
      # Nothing to plot without results, or if every artist's stats lookup failed
      if (results$status != "ok" || !test_numeric(results$artists$listeners, all.missing = FALSE)) {
        return(NULL)
      }
      chart <- apex(
        data = results$artists |> arrange(desc(listeners)),
        type = "bar",
        mapping = aes(x = name, y = listeners)
      ) |>
        # The searched genre, not input$genre, so changing the dropdown
        # without searching doesn't relabel the current chart
        ax_title(text = paste0("Top artists tagged '", results$genre, "' by Last.fm listeners")) |>
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
      if (test_function(open_artist)) {
        # Clicking a bar opens that artist's profile (see input$chart_click).
        # No highlight effect: the app switches to the profile straight away.
        chart <- set_input_click(chart, "chart_click", effect_type = "none")
      }
      chart
    })
  })
}
