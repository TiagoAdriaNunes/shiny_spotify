# Spotify Search App

The Spotify Search App is a Shiny application that allows users to search for artists, view their profiles, explore their top tracks, and discover related artists. Additionally, users can search for artists by genre and visualize the top artists in that genre.

This app is built using Rhino, a Shiny enterprise standard framework, which provides a structured and scalable way to develop Shiny applications.

The app is hosted at shinyapps.io: [https://tiagoadrianunes.shinyapps.io/shiny_spotify/](https://tiagoadrianunes.shinyapps.io/shiny_spotify/)

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
    git clone https://github.com/tiagoadrianunes/spotify-search-app.git
    cd spotify-search-app
    ```

2. **Create a .Renviron file with the Spotify API key/secret (which can be generated at the following URL: [https://developer.spotify.com/dashboard/create](https://developer.spotify.com/dashboard/create)):**
    ```r
    SPOTIFY_CLIENT_ID=your_client_id
    SPOTIFY_CLIENT_SECRET=your_client_secret
    ```

4. **Use renv to install all the packages, in the terminal use:**:
    ```r
    renv::restore()
    ```

5. **Run the app**:
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
    - Cache of the used requisitions with memoise to avoid new API calls for content that has already been accessed.


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


## Areas for Enhancement
- API Handling: At the moment, there is room to improve rate limit management and error handling to ensure a more robust experience.
- Content Expansion: Currently, trending artists and related information are not displayed but could be valuable additions.
Search Functionality: Introducing a search feature for songs would significantly enhance user interaction.

## Opportunities for Future Improvements
- Track Display: The app currently showcases the top 5 tracks, with potential to introduce pagination for broader exploration.
- Performance Optimization: The main artist page’s load time could be improved by optimizing how related artist data is fetched.
- Loading Indicators: Adding loaders would provide visual feedback during content loading, enhancing the user experience.
- UI Refinement: There is potential to further refine the interface to ensure a smoother and more polished presentation.
- Enhanced data mining on artists and songs: Analyze which genres feature the most popular artists or songs, and explore their characteristics such as tempo, danceability, and more.
- Graph Customization: The Apexcharter and artist network graph is functional but could benefit from additional customization to align data presentation more closely with the background.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

- This app uses the [Spotify Web API](https://developer.spotify.com/documentation/web-api/) to fetch artist data.
- Special thanks to the developers of the R packages used in this project.
