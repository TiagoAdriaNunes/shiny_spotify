# related_artists.R

box::use(
  dplyr[`%>%`],
  memoise[memoise],
  shiny[...], # nolint
  spotifyr[get_related_artists],
  visNetwork[renderVisNetwork, visEdges, visNetwork, visNetworkOutput, visNodes, visOptions],
)

# Memoize the Spotify API function for caching
get_related_artists_memo <- memoise(get_related_artists)

# UI function for the related artists
#' @export
ui <- function(id) {
  ns <- NS(id)
  visNetworkOutput(ns("related_artists_network"))
}

# Helper function to fetch related artists
fetch_related_artists <- function(artist_id) {
  tryCatch({
    get_related_artists_memo(artist_id)
  }, error = function(e) {
    NULL
  })
}

# Helper function to fetch related artists for multiple artists
related_artists_for_multiple <- function(artist_ids) {
  all_related_artists <- list()
  for (artist_id in artist_ids) {
    related_artists <- fetch_related_artists(artist_id)
    if (!is.null(related_artists)) {
      all_related_artists[[artist_id]] <- related_artists
    }
  }
  all_related_artists
}

# Helper function to render related artists network
render_related_artists_network <- function(ns, artist_id, artist_name, related_artists) { #nolint
  if (is.null(related_artists) ||
        !is.data.frame(related_artists) ||
        nrow(related_artists) == 0) {
    return(tags$p("No related artists found."))
  }
  # Limit to the top 5 related artists
  top_related_artists <- related_artists[seq_len(min(5, nrow(related_artists))), ]

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
    stringsAsFactors = FALSE
  )

  # Mapping from artist IDs to node IDs
  artist_id_to_node_id <- list()
  node_id_counter <- 1
  artist_id_to_node_id[[artist_id]] <- node_id_counter

  # Add main artist node
nodes <- rbind(nodes, data.frame(
  id = node_id_counter,
  label = artist_name,
  title = artist_name,
  stringsAsFactors = FALSE
))

  # Function to add artist to nodes if not already present
  add_artist_node <- function(artist_id_inner, artist_name_inner) {
    if (!artist_id_inner %in% names(artist_id_to_node_id)) {
      node_id_counter <<- node_id_counter + 1
      artist_id_to_node_id[[artist_id_inner]] <<- node_id_counter
      nodes <<- rbind(nodes, data.frame(
        id = node_id_counter,
        label = artist_name_inner,
        title = artist_name_inner,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Add top related artists
  for (i in seq_len(nrow(top_related_artists))) {
    related_artist <- top_related_artists[i, ]
    add_artist_node(related_artist$id, related_artist$name)
    edges <- rbind(edges, data.frame(
      from = artist_id_to_node_id[[artist_id]],  # From main artist
      to = artist_id_to_node_id[[related_artist$id]],
      stringsAsFactors = FALSE
    ))
  }

  # Fetch related artists for the top related artists
  all_related_artists <- related_artists_for_multiple(top_related_artists$id)

  # Add second-level related artists
  for (i in seq_len(nrow(top_related_artists))) {
    parent_artist_id <- top_related_artists$id[i]
    related_artists <- all_related_artists[[parent_artist_id]]

    if (!is.null(related_artists)) {
      # Limit to the top 5 related artists
      max_related <- min(5, nrow(related_artists))
      related_artists <- related_artists[1:max_related, ]
      for (j in seq_len(nrow(related_artists))) {
        related_artist <- related_artists[j, ]
        add_artist_node(related_artist$id, related_artist$name)
        edges <- rbind(edges, data.frame(
          from = artist_id_to_node_id[[parent_artist_id]],  # From parent artist
          to = artist_id_to_node_id[[related_artist$id]],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  visNetwork(nodes, edges) %>%
    visNodes(shape = "dot", size = 10, font = list(color = "white")) %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}

# Server function for the related artists
#' @export
server <- function(id, artist_id, artist_name = reactive(artist_name)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # React to artist_id changes
    observe({
      if (is.null(artist_id())) {
        output$related_artists_network <- renderUI({
          tags$p("No artist selected.")
        })
      } else {
        observeEvent(artist_id(), {
          req(artist_id())
          # Fetch related artists
          related_artists <- fetch_related_artists(artist_id())
          # Render related artists network
          output$related_artists_network <- renderVisNetwork({
            render_related_artists_network(ns, artist_id(), artist_name(), related_artists)
          })
        })
      }
    })
  })
}
