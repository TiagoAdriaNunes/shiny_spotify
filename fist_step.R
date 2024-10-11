library(spotifyr)

# Set up your Client ID and Secret
Sys.setenv(SPOTIFY_CLIENT_ID = '1fbe1c2f6e2e49679cc15d49e4932019')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'faf200a171c44133acec26fcb3873f5c')

# Retrieve access token
access_token <- get_spotify_access_token()

# Search for an artist
results <- search_spotify('Kendrick Lamar', type = 'artist')
print(results)

kendrick <- get_artist('2YZyLoL8N0Wb9xBt1NhZWg')

playlists <- get_category_playlists('pop', country = 'BR', limit = 10)

View(playlists)

playlist_tracks <- get_playlist_tracks(playlists$id[1])

trending_artists <- unique(playlist_tracks$track.artists)

brazil <- search_spotify("modão", market = "BR")

View(brazil)

spotifyr::get_artist()

kendrick_related <- spotifyr::get_related_artists('2YZyLoL8N0Wb9xBt1NhZWg')

indiepop <- spotifyr::get_genre_artists("indie-pop")
View(indiepop)

sertanejo <- spotifyr::get_genre_artists("sertanejo")

View(sertanejo)

chicago <- spotifyr::get_genre_artists("chicago rap")
View(chicago)


#tiago <- get_categories()     
