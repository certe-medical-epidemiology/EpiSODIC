// Render GitHub's alert syntax ("> [!WARNING]" and its siblings) as a
// Bootstrap alert on this site. GitHub draws those quotes as coloured
// boxes itself, but pandoc reads them as a plain quote beginning with
// the literal marker, so without this README.md could carry a callout
// that is a box on GitHub and a quote reading "[!WARNING]" here.
document.addEventListener("DOMContentLoaded", function () {
  var kinds = {
    NOTE: "info",
    TIP: "success",
    IMPORTANT: "primary",
    WARNING: "warning",
    CAUTION: "danger"
  };
  document.querySelectorAll("blockquote").forEach(function (quote) {
    var first = quote.querySelector("p");
    if (!first) {
      return;
    }
    var marker = first.innerHTML.match(/^\s*\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]\s*/);
    if (!marker) {
      return;
    }
    first.innerHTML = first.innerHTML.slice(marker[0].length);
    var box = document.createElement("div");
    box.className = "alert alert-" + kinds[marker[1]];
    box.setAttribute("role", "note");
    while (quote.firstChild) {
      box.appendChild(quote.firstChild);
    }
    quote.replaceWith(box);
  });
});
