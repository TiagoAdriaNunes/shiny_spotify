box::use(
  checkmate[
    assert_count,
    assert_list,
    assert_string,
    test_list
  ],
  purrr[map_chr],
  utils[head],
)

box::use(
  app / logic / lastfm[lastfm_api],
)

#' Get an artist's top Last.fm tags, used as genres now that Spotify no
#' longer returns `genres` for artists
#' @param artist The name of the artist
#' @param limit Maximum number of tags to return
#' @return A character vector of tag names, most-applied first

#' @export
get_artist_tags <- function(artist, limit = 5) {
  assert_string(artist, min.chars = 1)
  assert_count(limit, positive = TRUE)
  result <- lastfm_api("artist.getTopTags", params = list(artist = artist))
  head(parse_artist_tags(result), limit)
}

#' @export
parse_artist_tags <- function(result) {
  assert_list(result, null.ok = TRUE)
  tags <- result$toptags$tag
  # Covers an error response, a missing field and an artist with no tags
  if (!test_list(tags, min.len = 1)) {
    return(character(0))
  }
  map_chr(tags, "name")
}
