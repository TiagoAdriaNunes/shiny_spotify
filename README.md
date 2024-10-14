# Spotify Search App

The Spotify Search App is a Shiny application that allows users to search for artists, view their profiles, explore their top tracks, and discover related artists. Additionally, users can search for artists by genre and visualize the top artists in that genre.

## Features

### Artist Profile

- **Artist Search**: Users can search for an artist by name. The app uses the Spotify API to fetch artist information.
- **Artist Profile**: Displays detailed information about the selected artist, including their image, name, popularity, number of followers, and genres.
- **Top Tracks**: Lists the top tracks of the selected artist.
- **Related Artists**: Shows a network visualization of artists related to the selected artist.

### Search by Genre

- **Genre Filter**: Users can select a genre from a dropdown list and search for artists in that genre.
- **Top Artists by Genre**: Displays a table of the top artists in the selected genre, including their name, popularity, number of followers, and genres.
- **Followers Chart**: Visualizes the top 20 artists in the selected genre by the number of followers using a bar chart.

## Installation

To run the Spotify Search App locally, follow these steps:

1. **Clone the repository**:
    ```sh
    git clone https://github.com/yourusername/spotify-search-app.git
    cd spotify-search-app
    ```

2. **Install the required packages**:
    ```r
    install.packages(c("rhino", "renv"))
    ```

3. **Create a .Renviron file with the Spotify API key/secret (which can be generated at the following URL: [https://developer.spotify.com/dashboard/create](https://developer.spotify.com/dashboard/create)):**
    ```r
    SPOTIFY_CLIENT_ID=your_client_id
    SPOTIFY_CLIENT_SECRET=your_client_secret
    ```

4. **Install the required packages**:
    ```r
    install.packages(c("rhino", "renv"))
    ```

5. **Use renv to install all the packages, in the terminal use:**:
    ```r
    renv::restore()
    ```

6. **Run the app**:
    ```r
    rhino::runApp()
    ```

## File Structure

- **main.R**: The main file that defines the top-level UI and server functions.
- **artist_search.R**: Contains the UI and server logic for searching artists by name.
- **artist_profile.R**: Contains the UI and server logic for displaying the artist's profile.
- **artist_top_tracks.R**: Contains the UI and server logic for displaying the artist's top tracks.
- **related_artists.R**: Contains the UI and server logic for displaying related artists.
- **genre_filter.R**: Contains the UI and server logic for searching artists by genre and displaying the top artists in that genre.

## Features

1. **Search for an Artist**:
    - Navigate to the "Artist Profile" tab.
    - Enter the name of the artist in the search box and click "Search".
    - View the artist's profile, listen to top tracks, and see related artists in a network graph.

2. **Search by Genre**:
    - Navigate to the "Search by Genre" tab.
    - Select a genre from the dropdown list and click "Search".
    - View the table of artists of a select genre, popularity, number of followers and their genres.
    - View the top 20 artists graph in the selected genre and their follower counts.

3. **Cache**:
    - Chace of the used requisitons with memoise to don't use new api calls for a content who is alread acessed.

## Dependencies

- **shiny**: For building the web application.
- **bslib**: For theming and layout.
- **dplyr**: For data manipulation.
- **memoise**: For caching API calls.
- **reactable**: For rendering interactive tables.
- **apexcharter**: For creating charts.
- **spotifyr**: For interacting with the Spotify API.
- **visNetwork**: For creating network visualizations.
- **grDevices**: For color manipulation.
- **htmltools**: For HTML rendering.
- **scales**: For formatting numbers.

## Limitations and future fixing

- There is no API rate limit usage control or substantial error handling regarding the API.
- The main artist page loads slowly because it also loads data from related artists.
- The app needs loaders to indicate when content is being loaded.
- The UI, in general, needs more polishing.
- The apexcharter graph needs more customization to properly display the data aligned with the background.
- Proper handling of genre names is needed.
- Proper handling of genre calls is required to display more artists. Currently, only 50 genres per genre call are visible,  but this can be expanded by managing API calls and pagination.
- Currently, only the top 5 tracks are shown, but pagination can be added to display more tracks.
- The search function for artists needs more refinement to show related search terms based on the user’s query.
- Overall, the app needs better integration. For example, clicking a genre should display its artists on another tab, or    clicking an artist in the genre tab should redirect the user to the artist's profile page with their information loaded.
- There are no trending artists or related information displayed.
- There is no search functionality for songs.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

- This app uses the [Spotify Web API](https://developer.spotify.com/documentation/web-api/) to fetch artist data.
- Special thanks to the developers of the R packages used in this project.
