box::use(
  checkmate[
    assert_list,
    assert_string,
    test_data_frame
  ],
  dplyr[
    bind_rows,
    coalesce,
    slice_max,
    transmute
  ],
  purrr[
    keep,
    map,
    map_int,
    set_names
  ],
)

box::use(
  app / logic / spotify_api[get_artist_albums],
)

# Release groups accepted by Spotify's /v1/artists/{id}/albums, in display order
release_groups <- c("album", "single", "appears_on")

#' Count an artist's releases per group and find their latest release.
#' Spotify's `total` counts each edition (deluxe, remaster, ...) separately.
#' @param artist_id The Spotify ID of the artist
#' @return A list with `counts` (named integer vector) and `latest`
#'   (list with name, type, release_date and url, or NULL)

#' @export
get_release_summary <- function(artist_id) {
  assert_string(artist_id, min.chars = 1)
  # limit = 1 is enough: `total` covers the whole group and results come
  # newest first, so the single item is that group's latest release
  pages <- release_groups |>
    set_names() |>
    map(\(group) get_artist_albums(artist_id, include_groups = group, limit = 1))
  summarise_releases(pages)
}

#' @export
summarise_releases <- function(pages) {
  assert_list(pages, names = "unique")
  counts <- map_int(pages, \(page) as.integer(coalesce(page$total, 0L)))
  # Only the artist's own albums and singles count as their latest release
  candidates <- pages[intersect(names(pages), c("album", "single"))] |>
    map("items") |>
    keep(\(items) test_data_frame(items, min.rows = 1)) |>
    bind_rows()
  latest <- if (nrow(candidates) > 0) {
    candidates |>
      # release_date is "YYYY", "YYYY-MM" or "YYYY-MM-DD", which sort as strings
      slice_max(release_date, n = 1, with_ties = FALSE) |>
      transmute(name, type = album_type, release_date, url = external_urls.spotify) |>
      as.list()
  }
  list(counts = counts, latest = latest)
}
