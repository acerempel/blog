function setColourScheme(prefix, colourScheme, element) {
  var classes = element.getAttribute("class").split(" ");
  var newClasses = classes.filter(className => !className.startsWith(prefix));
  var colourSchemeClassName = prefix + colourScheme;
  newClasses.push(colourSchemeClassName);
  element.setAttribute("class", newClasses.join(" "));
}

function changeColourScheme(prefix, selector) {
  var element = document.querySelector(selector);
  setColourScheme(prefix, event.target.value, element);
}
