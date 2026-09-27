box::use(
  checkmate[
    assert_count,
    assert_list,
    assert_string,
    test_list,
    test_null
  ],
  purrr[map_chr],
)

box::use(
  app / logic / lastfm[lastfm_api],
)

#' Get similar artists from Last.fm API and format the response
#' @param artist The name of the artist to find similar artists for
#' @param limit Optional limit for the number of similar artists to return
#' @return A data frame with similar artists' names and match scores

#' @export
get_similar_artists_formatted <- function(artist, limit = 5) {
  assert_string(artist, min.chars = 1)
  assert_count(limit, positive = TRUE, null.ok = TRUE)
  # api_key, method and format are added by lastfm_api()
  params <- list(artist = artist)
  # Add limit if specified
  if (!test_null(limit)) {
    params$limit <- limit
  }
  result <- lastfm_api(
    "artist.getSimilar",
    params = params
  )
  parse_similar_artists(result)
}

#' @export
parse_similar_artists <- function(result) {
  assert_list(result, null.ok = TRUE)
  artists <- result$similarartists$artist
  # Covers both a missing field (NULL) and an empty result (list())
  if (!test_list(artists, min.len = 1)) {
    return(NULL)
  }
  data.frame(
    name = map_chr(artists, "name"),
    match = as.numeric(map_chr(artists, "match"))
  )
}
