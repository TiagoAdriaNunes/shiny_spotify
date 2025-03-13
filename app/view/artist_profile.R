# artist_profile.R

# Import necessary libraries and functions
box::use(
  bslib[breakpoints, card, layout_columns, page_fillable],
  grDevices[colorRampPalette],
  htmltools[HTML],
  memoise[memoise],
  scales[comma],
  shiny[...], # nolint
  spotifyr[get_artist],
)

# Memoize the Spotify API function for caching
get_artist_memo <- memoise(get_artist)

generate_svg_circle <- function(popularity_value) {
  popularity_value <- as.numeric(popularity_value)
  # Calculate the radius of the circle
  radius <- 10 + 15 * (popularity_value / 100)
  # Interpolate the color from red (popularity = 0) to green (popularity = 100)
  circle_colour_picker <- colorRampPalette(c("#B91d1d", "#ED8E11", "#EDDE11", "#1DB954"))
  # There are 101 colour values since popularity ranges from 0 to 100
  color <- circle_colour_picker(101)[popularity_value + 1]
  # Generate the SVG code for the circle
  svg_code <- sprintf(
    '<svg height="%1$s" width="%6$s"><circle cx="%3$s" cy="%2$s" r="%2$s" stroke="none" stroke-width="0" fill="%5$s" /><text class="circle-text" x="%3$s" y="%2$s" font-size="%4$s" fill="white" text-anchor="middle" dy=".3em">%7$s</text></svg>', # nolint
    2 * radius, # SVG height
    radius, # Circle center y
    radius + 80, # Circle center x (shifted to the right)
    radius * 0.6, # Font size based on radius
    color, # Fill color used also for stroke
    2 * radius + 100, # SVG width to accommodate the text
    popularity_value # Text to display inside the circle
  )
}

# UI function for the artist profile
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      card(
        htmlOutput(ns("artist_image")),
        tags$h3(textOutput(ns("artist_name"))),
        tags$div(
          style = "display: flex; align-items: center;",
          htmlOutput(ns("artist_popularity_circle"))
        ),
        tags$p(textOutput(ns("artist_followers"))),
        tags$p(textOutput(ns("artist_genres")))
      ),
      col_widths = breakpoints(
        sm = c(6),
        md = c(12),
        lg = c(12)
      )
    )
  )
}

# Helper function to fetch artist data
fetch_artist_data <- function(artist_id) {
  tryCatch(
    {
      get_artist_memo(artist_id)
    },
    error = function(e) {
      NULL
    }
  )
}

# Server function for the artist profile
#' @export
server <- function(id, artist_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # React to artist_id changes
    observeEvent(artist_id(), {
      req(artist_id())
      # Fetch artist data
      artist_info <- fetch_artist_data(artist_id())
      # Render artist's image dynamically (only the second image) and center it
      output$artist_image <- renderUI({
        if (!is.null(artist_info$images) && length(artist_info$images$url) > 1) {
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
        if (!is.null(artist_info$name)) {
          paste(artist_info$name)
        } else {
          "Name not available."
        }
      })
      # Render artist's popularity circle with "Popularity:" text
      output$artist_popularity_circle <- renderUI({
        if (!is.null(artist_info$popularity)) {
          tags$div(
            style = "display: flex; align-items: center;",
            tags$p("Popularity:", style = "margin-right: 10px;"),
            HTML(generate_svg_circle(artist_info$popularity))
          )
        } else {
          "Popularity not available."
        }
      })
      # Render artist's followers
      output$artist_followers <- renderText({
        if (!is.null(artist_info$followers$total)) {
          paste("Followers:", comma(artist_info$followers$total))
        } else {
          "Followers not available."
        }
      })
      # Render artist's genres
      output$artist_genres <- renderText({
        if (!is.null(artist_info$genres) && length(artist_info$genres) > 0) {
          paste("Genres:", paste(artist_info$genres, collapse = ", "))
        } else {
          "Genres not available."
        }
      })
    })
  })
}
