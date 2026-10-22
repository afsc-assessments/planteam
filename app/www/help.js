// This page does not read or change document content or working files.
document.getElementById("help-return").addEventListener("click", event => {
  try {
    if (window.opener && !window.opener.closed && window.opener.location.origin === window.location.origin) {
      event.preventDefault();
      window.opener.focus();
      window.close();
      // If the browser prevents close/focus, retain an explicit, usable fallback.
      document.getElementById("return-message").hidden = false;
    }
  } catch (_) {
    // A direct visit or unavailable opener uses the ordinary editor link.
  }
});
