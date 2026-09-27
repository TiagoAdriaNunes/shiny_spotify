box::use(
  bslib[
    breakpoints,
    card,
    layout_columns,
    page_fillable,
    tooltip
  ],
  checkmate[
    test_character,
    test_function,
    test_list,
    test_string
  ],
  dplyr[coalesce],
  memoise[memoise],
  purrr[
    imap,
    map
  ],
  shiny[
    htmlOutput,
    moduleServer,
    NS,
    observeEvent,
    reactive,
    renderText,
    renderUI,
    req,
    tags,
    textOutput
  ],
)

box::use(
  app / logic / artist_releases[get_release_summary],
  app / logic / get_artist_tags[get_artist_tags],
  app / logic / spotify_api[get_artist],
)

# Memoize the API functions for caching
get_artist_memo <- memoise(get_artist)
get_release_summary_memo <- memoise(get_release_summary)
get_artist_tags_memo <- memoise(get_artist_tags)

# Display labels for each release group, in display order
release_labels <- c(
  album = "Albums",
  single = "Singles & EPs",
  appears_on = "Featured on"
)

# Hover text explaining what each count includes. Spotify counts every
# edition (deluxe, remaster, ...) as a separate release.
release_descriptions <- c(
  album = "The artist's own albums on Spotify, counting each edition separately",
  single = "The artist's own singles and EPs on Spotify",
  appears_on = "Releases by other artists that include this artist, such as guest features and compilations"
)

release_type_labels <- c(album = "Album", single = "Single")

# UI function for the artist profile
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      card(
        htmlOutput(ns("artist_image")),
        tags$h3(textOutput(ns("artist_name"))),
        htmlOutput(ns("artist_link")),
        htmlOutput(ns("artist_releases")),
        htmlOutput(ns("artist_genres"))
      ),
      col_widths = breakpoints(
        sm = c(6),
        md = c(12),
        lg = c(12)
      )
    )
  )
}

# Call `fetch`, turning any API failure into NULL so one broken source
# doesn't blank the whole profile
fetch_or_null <- function(fetch, ...) {
  tryCatch(fetch(...), error = function(e) NULL)
}

#' @export
render_release_stats <- function(releases) {
  if (!test_list(releases) || length(releases$counts) == 0) {
    return(tags$p("Releases not available."))
  }
  counts <- releases$counts[intersect(names(release_labels), names(releases$counts))]
  stats <- tags$div(
    class = "release-stats",
    imap(counts, function(count, group) {
      # A Bootstrap tooltip rather than a `title` attribute: native title
      # tooltips are slow, hidden on touch screens and don't show in some
      # viewers (e.g. RStudio's)
      tooltip(
        tags$div(
          class = "release-stat",
          tabindex = "0",
          tags$span(class = "release-stat-count", count),
          tags$span(class = "release-stat-label", release_labels[[group]])
        ),
        release_descriptions[[group]]
      )
    })
  )
  latest <- releases$latest
  if (test_list(latest) && test_string(latest$name, min.chars = 1)) {
    type <- release_type_labels[coalesce(latest$type, "")]
    details <- c(if (!is.na(type)) type, substr(latest$release_date, 1, 4))
    stats <- tags$div(
      stats,
      tags$p(
        class = "latest-release",
        "Latest release: ",
        tags$a(href = latest$url, target = "_blank", latest$name),
        paste0(" · ", details, collapse = "")
      )
    )
  }
  stats
}

#' @export
render_genre_tags <- function(genres, input_id = NULL) {
  if (!test_character(genres, min.len = 1)) {
    return(tags$p("Genres not available."))
  }
  tags$div(
    tags$div(class = "genre-tags", map(genres, function(genre) {
      if (!test_string(input_id)) {
        return(tags$span(class = "genre-tag", genre))
      }
      tags$button(
        type = "button",
        class = "genre-tag genre-search-link",
        `data-input-id` = input_id,
        `data-genre` = genre,
        title = paste("Find artists tagged", genre),
        genre
      )
    })),
    tags$small(class = "text-muted", "Genres from Last.fm")
  )
}

# Server function for the artist profile
#' @export
server <- function(id, artist_id, open_genre = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$genre_clicked, {
      req(test_function(open_genre), test_string(input$genre_clicked, min.chars = 1))
      open_genre(input$genre_clicked)
    })
    # The API calls live in reactives that the outputs read, so while one is
    # running its outputs are marked as recalculating and show a spinner
    artist_info <- reactive({
      req(artist_id())
      fetch_or_null(get_artist_memo, artist_id())
    })
    releases <- reactive({
      req(artist_id())
      fetch_or_null(get_release_summary_memo, artist_id())
    })
    # Spotify no longer returns genres, so use the artist's Last.fm tags
    genres <- reactive({
      name <- artist_info()$name
      if (test_string(name, min.chars = 1)) {
        fetch_or_null(get_artist_tags_memo, name)
      }
    })
    # Render artist's image dynamically (only the second image) and center it
    output$artist_image <- renderUI({
      urls <- artist_info()$images$url
      if (test_character(urls, min.len = 2)) {
        tags$div(
          class = "artist-image",
          # Spotify's second image is 320px square. Giving the size up front
          # reserves its space, so the card doesn't jump when it downloads;
          # .artist-image (see main.scss) lets it shrink on narrow cards.
          tags$img(
            src = urls[2],
            alt = artist_info()$name,
            width = 320,
            height = 320
          )
        )
      } else {
        tags$p("Image not available.")
      }
    })
    # Render artist's name
    output$artist_name <- renderText({
      name <- artist_info()$name
      if (test_string(name, min.chars = 1)) {
        name
      } else {
        "Name not available."
      }
    })
    # Render a link to the artist on Spotify
    output$artist_link <- renderUI({
      url <- artist_info()$external_urls$spotify
      if (test_string(url, min.chars = 1)) {
        tags$a(href = url, target = "_blank", class = "spotify-link", "Open in Spotify")
      }
    })
    # Render the release counts and latest release
    output$artist_releases <- renderUI({
      render_release_stats(releases())
    })
    # Render the artist's genres
    output$artist_genres <- renderUI({
      render_genre_tags(genres(), input_id = if (test_function(open_genre)) session$ns("genre_clicked"))
    })
  })
}
