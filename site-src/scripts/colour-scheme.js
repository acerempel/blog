var prefix = "colour-scheme-";
var storageKey = "colour-scheme";
var colourSchemeSelectId = "colour-scheme-select";

function setColourScheme(element, colourScheme) {
  var classes = element.getAttribute("class").split(" ");
  var newClasses = classes.filter(className => !className.startsWith(prefix));
  var colourSchemeClassName = prefix + colourScheme;
  newClasses.push(colourSchemeClassName);
  element.setAttribute("class", newClasses.join(" "));
  window.localStorage.setItem(storageKey, colourScheme);
}

function addSelectOnChangeEvent(element, select) {
  select.onchange = function(event) {
    setColourScheme(element, event.target.value);
  }
}

function getSavedColourScheme(element, select) {
  var stored = window.localStorage.getItem(storageKey);
  if (stored) {
    select.value = stored;
    setColourScheme(element, stored);
  }
};

// This script should have the 'defer' attribute set, so that the
// 'DOMContentLoaded' event will not yet have fired when it is run.
document.addEventListener("DOMContentLoaded", function(_event) {
  var select = document.getElementById(colourSchemeSelectId);
  var body = document.querySelector("body");
  addSelectOnChangeEvent(body, select);
  getSavedColourScheme(body, select);
});
