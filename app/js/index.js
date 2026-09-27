// Pressing Enter in the artist search box runs the search, the same as
// clicking its Search button. The text input only sends its value to the
// server on "change", so that's fired first: the button's click then reaches
// the server together with what was just typed.
document.addEventListener('keydown', (event) => {
  // isComposing: Enter that confirms an IME composition (e.g. Japanese
  // input) isn't a request to search
  if (event.key !== 'Enter' || event.isComposing) return;
  const input = event.target;
  if (input.tagName !== 'INPUT') return;
  const search = input.closest('.artist-search');
  if (!search) return;
  // The Search button specifically: the box's clear (×) is a button too
  const button = search.querySelector('.bslib-task-button');
  // Disabled while a search is already running
  if (!button || button.disabled) return;
  event.preventDefault();
  input.dispatchEvent(new Event('change', { bubbles: true }));
  button.click();
});

// The × in the artist search box empties it and puts the cursor back in it,
// ready to type. "change" tells Shiny the box is now empty. The artist on
// screen stays loaded; only the text is cleared.
document.addEventListener('click', (event) => {
  const clear = event.target.closest('.search-clear');
  if (!clear) return;
  const input = clear.parentElement.querySelector('input');
  if (!input) return;
  input.value = '';
  input.dispatchEvent(new Event('change', { bubbles: true }));
  input.focus();
});

// The genre dropdown (a single-choice selectize). Once a genre is chosen,
// selectize ignores typing until that choice is removed, so typing a second
// genre did nothing: the first stayed selected and was searched again.
// Opening the dropdown now empties it (silently, so the server keeps the
// current genre) and focuses it, so typing filters straight away; closing it
// without a new choice puts the previous genre back. Used as the dropdown's
// onDropdownOpen / onDropdownClose options in genre_filter.R, where `this` is
// the selectize instance.
export function genreDropdownOpen() {
  this.previousGenre = this.getValue();
  if (this.previousGenre) {
    this.clear(true);
    this.focus();
  }
}

export function genreDropdownClose() {
  if (!this.getValue() && this.previousGenre) {
    this.setValue(this.previousGenre, true);
  }
}

// Artist names in the genre results table open that artist's profile. Each
// name is a button carrying the Shiny input to notify (data-input-id) and the
// artist's name (data-artist); see genre_artist_cell() in genre_filter.R.
// priority "event" sends it even when the same artist is clicked twice.
document.addEventListener('click', (event) => {
  const button = event.target.closest('.genre-artist-link');
  if (!button || !window.Shiny) return;
  window.Shiny.setInputValue(button.dataset.inputId, button.dataset.artist, { priority: 'event' });
});
