box::use(
  checkmate[assert_character, assert_string, test_data_frame, test_function, test_string],
  memoise[memoise],
  purrr[compact, map, set_names],
  shiny[actionButton, moduleServer, NS, observeEvent, reactiveVal, renderUI, req, tags, uiOutput],
  stringr[str_glue],
  visNetwork[visEdges, visEvents, visInteraction, visNetwork, visNodes, visOptions],
)

box::use(
  app / logic / get_similar_artists[get_similar_artists_formatted],
)

# Memoize the formatted function for caching
get_similar_artists_memo <- memoise(get_similar_artists_formatted)

# Node tooltip style, matching the app's dark theme. visNetwork's default sets
# a light background but its text colour as `font-color`, which isn't a CSS
# property, so the text inherited the theme's white: white on cream. The
# style must keep `position: fixed; visibility: hidden` for visNetwork to
# position and toggle the tooltip.
tooltip_style <- paste(
  "position: fixed;",
  "visibility: hidden;",
  "padding: 5px 8px;",
  "font-size: 14px;",
  "color: #E0E0E0;",
  "background-color: #1F1F1F;",
  "border: 1px solid #444444;",
  "border-radius: 4px;",
  "box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.4);",
  "max-width: 400px;",
  "word-break: break-word;"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # The "Open profile" button sits over the network's top-right corner (see
  # .network-actions in main.scss), so it appearing doesn't move the network
  tags$div(
    class = "related-network",
    uiOutput(ns("related_artists_network")),
    uiOutput(ns("node_actions"), class = "network-actions")
  )
}

# Helper function to fetch similar artists from Last.fm (cached via memoise)
fetch_similar_artists <- function(artist_name) {
  assert_string(artist_name, min.chars = 1)
  tryCatch(
    get_similar_artists_memo(artist_name, limit = 5),
    error = function(e) NULL
  )
}

# Helper function to fetch similar artists for multiple artists
similar_artists_for_multiple <- function(artist_names) {
  assert_character(artist_names, min.chars = 1, any.missing = FALSE)
  artist_names |>
    set_names() |>
    map(fetch_similar_artists) |>
    compact()
}

# Helper function to render related artists network
#' @export
render_similar_artists_network <- function(ns, main_artist_name, similar_artists) {
  assert_string(main_artist_name, min.chars = 1)
  if (!test_data_frame(similar_artists, min.rows = 1)) {
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
    if (test_data_frame(second_level, min.rows = 1)) {
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
  # Sized explicitly because it's rendered through renderUI: without a
  # visNetworkOutput() container the widget falls back to a fixed 960px
  # width, wider than the card, which adds a horizontal scrollbar
  network <- visNetwork(nodes, edges, width = "100%", height = "400px") |>
    visNodes(shape = "dot",
             size = 10,
             font = list(color = "white")) |>
    visEdges(arrows = "to", width = ~ weight * 5) |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visInteraction(tooltipStyle = tooltip_style)
  if (test_function(ns)) {
    network <- report_selected_node(network, ns("selected_artist"))
  }
  network
}

# Report the clicked node's artist name to the Shiny input `input_id`, and
# NULL when it's deselected, so the server can offer to open their profile.
# `this` is the vis network, whose nodes hold each artist's name as `label`.
report_selected_node <- function(network, input_id) {
  send <- function(value) {
    as.character(str_glue(
      "function(params) {{ Shiny.setInputValue('{input_id}', {value}, {{ priority: 'event' }}); }}"
    ))
  }
  visEvents(
    network,
    selectNode = send("this.body.data.nodes.get(params.nodes[0]).label"),
    deselectNode = send("null")
  )
}

#' @export
#' @param open_artist Function taking an artist name that opens their profile
#'   and returns TRUE, or FALSE when Spotify has no match (see
#'   artist_search's server). NULL means no "Open profile" button.
server <- function(id, artist_name, open_artist = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # A single output that re-renders whenever the selected artist changes.
    # Fetching inside it marks the card as recalculating, so it shows a
    # spinner while Last.fm is queried. renderUI rather than renderVisNetwork:
    # the result is either the network or a message.
    output$related_artists_network <- renderUI({
      name <- artist_name()
      if (!test_string(name, min.chars = 1)) {
        return(tags$p("No artist selected."))
      }
      # Only report node clicks when there's somewhere to open them
      render_similar_artists_network(if (test_function(open_artist)) ns, name, fetch_similar_artists(name))
    })

    # Clicking a node offers to open that artist's profile. Selecting stays
    # separate from opening so clicking still just highlights connections.
    selected_artist <- reactiveVal(NULL)
    open_error <- reactiveVal("")
    observeEvent(input$selected_artist, ignoreNULL = FALSE, {
      selected_artist(input$selected_artist)
      open_error("")
    })
    # A new network starts with nothing selected
    observeEvent(artist_name(), {
      selected_artist(NULL)
      open_error("")
    })

    output$node_actions <- renderUI({
      if (test_string(open_error(), min.chars = 1)) {
        return(tags$p(class = "network-message", open_error()))
      }
      name <- selected_artist()
      # Nothing to offer for no selection, or for the artist already shown
      if (!test_string(name, min.chars = 1) || identical(name, artist_name())) {
        return(NULL)
      }
      actionButton(
        ns("open_selected"),
        as.character(str_glue("Open {name}'s profile")),
        class = "btn-sm"
      )
    })

    observeEvent(input$open_selected, {
      name <- selected_artist()
      req(test_function(open_artist), test_string(name, min.chars = 1))
      if (!open_artist(name)) {
        open_error(as.character(str_glue("'{name}' wasn't found on Spotify.")))
      }
    })
  })
}
