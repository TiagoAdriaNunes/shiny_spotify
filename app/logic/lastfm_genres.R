box::use(
  checkmate[
    assert_count,
    assert_data_frame,
    assert_function,
    assert_list,
    assert_string,
    test_list
  ],
  dplyr[
    coalesce,
    mutate,
    tibble
  ],
  purrr[
    map,
    map_chr,
    map_dbl,
    map_int
  ],
)

box::use(
  app / logic / lastfm[lastfm_api, stop_for_lastfm_error],
)

# Popular Last.fm tags that describe the listener or their collection rather
# than the music, so they're left out of the genre list
non_genre_tags <- c(
  "albums i own",
  "awesome",
  "beautiful",
  "bookmark",
  "cover",
  "covers",
  "favorite",
  "favorites",
  "favourite",
  "favourites",
  "female vocalist",
  "female vocalists",
  "love",
  "male vocalist",
  "male vocalists",
  "mellow",
  "seen live"
)

#' Get the most used Last.fm tags, as a list of genres to browse
#' @param limit Number of top tags to request; a few are dropped as non-genres
#' @return A character vector of tag names, most used first

#' @export
get_top_genres <- function(limit = 100) {
  assert_count(limit, positive = TRUE)
  result <- lastfm_api("chart.getTopTags", params = list(limit = limit))
  stop_for_lastfm_error(result)
  parse_top_genres(result)
}

#' @export
parse_top_genres <- function(result) {
  assert_list(result, null.ok = TRUE)
  tags <- result$tags$tag
  if (!test_list(tags, min.len = 1)) {
    return(character(0))
  }
  genres <- map_chr(tags, "name")
  genres[!tolower(genres) %in% non_genre_tags]
}

#' Get the top artists Last.fm listeners have tagged with a genre
#' @param genre A Last.fm tag, e.g. "rock"
#' @param limit Maximum number of artists to return
#' @return A tibble with rank, name and url, or NULL when nothing matches

#' @export
get_genre_artists <- function(genre, limit = 20) {
  assert_string(genre, min.chars = 1)
  assert_count(limit, positive = TRUE)
  result <- lastfm_api("tag.getTopArtists", params = list(tag = genre, limit = limit))
  stop_for_lastfm_error(result)
  parse_genre_artists(result)
}

#' @export
parse_genre_artists <- function(result) {
  assert_list(result, null.ok = TRUE)
  artists <- result$topartists$artist
  # Last.fm answers an unknown tag with an empty list rather than an error
  if (!test_list(artists, min.len = 1)) {
    return(NULL)
  }
  tibble(
    rank = map_int(artists, \(artist) as.integer(artist$`@attr`$rank)),
    name = map_chr(artists, "name"),
    url = map_chr(artists, "url")
  )
}

#' Get an artist's Last.fm listener and play counts
#' @param artist The name of the artist
#' @return A list with numeric `listeners` and `playcount` (NA when missing)

#' @export
get_artist_stats <- function(artist) {
  assert_string(artist, min.chars = 1)
  result <- lastfm_api("artist.getInfo", params = list(artist = artist))
  stop_for_lastfm_error(result)
  parse_artist_stats(result)
}

#' @export
parse_artist_stats <- function(result) {
  assert_list(result, null.ok = TRUE)
  stats <- result$artist$stats
  # Play counts can pass 2^31, so keep them as doubles rather than integers
  list(
    listeners = as.numeric(coalesce(stats$listeners, NA_character_)),
    playcount = as.numeric(coalesce(stats$playcount, NA_character_))
  )
}

#' Add listener and play counts to a table of artists. tag.getTopArtists
#' doesn't return them, so this costs one artist.getInfo call per artist.
#' @param artists A data frame with a `name` column
#' @param get_stats Function returning stats for one artist name, e.g. a
#'   memoised get_artist_stats()
#' @return `artists` with `listeners` and `playcount` columns; an artist whose
#'   lookup fails gets NA rather than failing the whole table

#' @export
add_artist_stats <- function(artists, get_stats = get_artist_stats) {
  assert_data_frame(artists)
  assert_function(get_stats)
  missing_stats <- list(listeners = NA_real_, playcount = NA_real_)
  stats <- map(artists$name, \(name) tryCatch(get_stats(name), error = \(e) missing_stats))
  artists |>
    mutate(
      listeners = map_dbl(stats, "listeners"),
      playcount = map_dbl(stats, "playcount")
    )
}
