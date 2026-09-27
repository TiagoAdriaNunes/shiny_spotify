box::use(
  bslib[
    breakpoints,
    card,
    layout_columns,
    page_fillable
  ],
  checkmate[
    test_character,
    test_list,
    test_string
  ],
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
  appears_on = "Appears on"
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
      tags$div(
        class = "release-stat",
        tags$span(class = "release-stat-count", count),
        tags$span(class = "release-stat-label", release_labels[[group]])
      )
    })
  )
  latest <- releases$latest
  if (test_list(latest) && test_string(latest$name, min.chars = 1)) {
    type <- release_type_labels[latest$type %||% ""]
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
render_genre_tags <- function(genres) {
  if (!test_character(genres, min.len = 1)) {
    return(tags$p("Genres not available."))
  }
  tags$div(
    tags$div(class = "genre-tags", map(genres, \(genre) tags$span(class = "genre-tag", genre))),
    tags$small(class = "text-muted", "Genres from Last.fm")
  )
}

# Server function for the artist profile
#' @export
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    # React to artist_id changes
    observeEvent(artist_id(), {
      req(artist_id())
      artist_info <- fetch_or_null(get_artist_memo, artist_id())
      releases <- fetch_or_null(get_release_summary_memo, artist_id())
      # Spotify no longer returns genres, so use the artist's Last.fm tags
      genres <- if (test_string(artist_info$name, min.chars = 1)) {
        fetch_or_null(get_artist_tags_memo, artist_info$name)
      }
      # Render artist's image dynamically (only the second image) and center it
      output$artist_image <- renderUI({
        if (test_character(artist_info$images$url, min.len = 2)) {
          tags$div(
            style = "text-align: center;",
            tags$img(
              src = artist_info$images$url[2],
              style = "max-width: 100%; height: auto; width: auto\\9;"
            )
          )
        } else {
          tags$p("Image not available.")
        }
      })
      # Render artist's name
      output$artist_name <- renderText({
        if (test_string(artist_info$name, min.chars = 1)) {
          artist_info$name
        } else {
          "Name not available."
        }
      })
      # Render a link to the artist on Spotify
      output$artist_link <- renderUI({
        url <- artist_info$external_urls$spotify
        if (test_string(url, min.chars = 1)) {
          tags$a(href = url, target = "_blank", class = "spotify-link", "Open in Spotify")
        }
      })
      # Render the release counts and latest release
      output$artist_releases <- renderUI({
        render_release_stats(releases)
      })
      # Render the artist's genres
      output$artist_genres <- renderUI({
        render_genre_tags(genres)
      })
    })
  })
}
