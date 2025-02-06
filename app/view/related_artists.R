# related_artists.R

box::use(
  dplyr[`%>%`], memoise[memoise], shiny[...], # nolint
  visNetwork[renderVisNetwork, visEdges, visNetwork, visNetworkOutput, visNodes, visOptions], utils[str],
)

box::use(
  app / logic / get_similar_artists[get_similar_artists_formatted],
  app /logic / lastfm[lastfm_api],
)

# Memoize the formatted function for caching
get_similar_artists_memo <- memoise(get_similar_artists_formatted)

#' @export
ui <- function(id) {
  ns <- NS(id)
  visNetworkOutput(ns("related_artists_network"))
}

# Helper function to fetch similar artists from Last.fm
fetch_similar_artists <- function(artist_name) {
  tryCatch({
    result <- lastfm_api("artist.getSimilar", list(artist = artist_name, limit = 5))
    if (!is.null(result$similarartists$artist)) {
      # Extract only the relevant information
      similar_artists <- data.frame(
        name = vapply(result$similarartists$artist, function(x) {
          x$name
        }, character(1)),
        match = as.numeric(
          vapply(result$similarartists$artist, function(x) {
            x$match
          }, character(1))
        )
      )
      return(similar_artists)
    }
    return(NULL)
  }, error = function(e) {
    NULL
  })
}

# Helper function to fetch similar artists for multiple artists
similar_artists_for_multiple <- function(artist_names) {
  all_similar_artists <- list()
  for (artist_name in artist_names) {
    similar_artists <- fetch_similar_artists(artist_name)
    if (!is.null(similar_artists)) {
      all_similar_artists[[artist_name]] <- similar_artists
    }
  }
  all_similar_artists
}

# Helper function to render related artists network
render_similar_artists_network <- function(ns, main_artist_name, similar_artists) {
  if (is.null(similar_artists)) {
    return(tags$p("No similar artists found."))
  }
  # Initialize nodes and edges data frames
  nodes <- data.frame(
    id = integer(0),
    label = character(0),
    title = character(0),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = integer(0),
    to = integer(0),
    weight = numeric(0),
    stringsAsFactors = FALSE
  )
  # Mapping from artist names to node IDs
  artist_name_to_node_id <- list()
  node_id_counter <- 1
  # Add main artist node
  artist_name_to_node_id[[main_artist_name]] <- node_id_counter
  nodes <- rbind(
    nodes,
    data.frame(
      id = node_id_counter,
      label = main_artist_name,
      title = main_artist_name,
      stringsAsFactors = FALSE
    )
  )
  # Add first level similar artists
  for (i in seq_len(nrow(similar_artists))) {
    node_id_counter <- node_id_counter + 1
    similar_artist_name <- similar_artists$name[i]
    artist_name_to_node_id[[similar_artist_name]] <- node_id_counter
    # Add node
    nodes <- rbind(
      nodes,
      data.frame(
        id = node_id_counter,
        label = similar_artist_name,
        title = similar_artist_name,
        stringsAsFactors = FALSE
      )
    )
    # Add edge
    edges <- rbind(
      edges,
      data.frame(
        from = 1,
        # Main artist's ID is always 1
        to = node_id_counter,
        weight = similar_artists$match[i],
        stringsAsFactors = FALSE
      )
    )
    # Get second level similar artists
    second_level <- fetch_similar_artists(similar_artist_name)
    if (!is.null(second_level)) {
      for (j in seq_len(nrow(second_level))) {
        second_artist_name <- second_level$name[j]
        if (!second_artist_name %in% names(artist_name_to_node_id)) {
          node_id_counter <- node_id_counter + 1
          artist_name_to_node_id[[second_artist_name]] <- node_id_counter
          # Add node
          nodes <- rbind(
            nodes,
            data.frame(
              id = node_id_counter,
              label = second_artist_name,
              title = second_artist_name,
              stringsAsFactors = FALSE
            )
          )
          # Add edge
          edges <- rbind(
            edges,
            data.frame(
              from = artist_name_to_node_id[[similar_artist_name]],
              to = node_id_counter,
              weight = second_level$match[j],
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
  visNetwork(nodes, edges) %>%
    visNodes(shape = "dot",
             size = 10,
             font = list(color = "white")) %>%
    visEdges(arrows = "to", width = ~ weight * 5) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}

#' @export
server <- function(id, artist_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      if (is.null(artist_name())) {
        output$related_artists_network <- renderUI({
          tags$p("No artist selected.")
        })
      } else {
        observeEvent(artist_name(), {
          req(artist_name())
          # Fetch similar artists
          similar_artists <- fetch_similar_artists(artist_name())
          # Render similar artists network
          output$related_artists_network <- renderVisNetwork({
            render_similar_artists_network(ns, artist_name(), similar_artists)
          })
        })
      }
    })
  })
}
