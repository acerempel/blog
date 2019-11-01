function setColourScheme(prefix, colourScheme, element) {
  var classes = element.getAttribute("class").split(" ");
  var newClasses = classes.filter(className => !className.startsWith(prefix));
  var colourSchemeClassName = prefix + colourScheme;
  newClasses.push(colourSchemeClassName);
  element.setAttribute("class", newClasses.join(" "));
  window.localStorage.setItem("colour-scheme", colourScheme);
}

function changeColourScheme(prefix, selector) {
  var element = document.querySelector(selector);
  setColourScheme(prefix, event.target.value, element);
}

// This script should have the 'defer' attribute set, so that the
// 'DOMContentLoaded' event will not yet have fired when it is run.
document.addEventListener("DOMContentLoaded", function(_event) {
  var select = document.getElementById("colour-scheme-select");
  var stored = window.localStorage.getItem("colour-scheme");
  if (stored) { select.value = stored }
});
