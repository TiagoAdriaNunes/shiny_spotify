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
  const button = search.querySelector('button');
  // Disabled while a search is already running
  if (!button || button.disabled) return;
  event.preventDefault();
  input.dispatchEvent(new Event('change', { bubbles: true }));
  button.click();
});
