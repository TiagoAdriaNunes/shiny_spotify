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

// Artist names in the genre results table open that artist's profile. Each
// name is a button carrying the Shiny input to notify (data-input-id) and the
// artist's name (data-artist); see genre_artist_cell() in genre_filter.R.
// priority "event" sends it even when the same artist is clicked twice.
document.addEventListener('click', (event) => {
  const button = event.target.closest('.genre-artist-link');
  if (!button || !window.Shiny) return;
  window.Shiny.setInputValue(button.dataset.inputId, button.dataset.artist, { priority: 'event' });
});
