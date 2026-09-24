document.addEventListener("keydown", function (event) {
  var target = event.target;
  var isEditable = target.matches("input, textarea, select, [contenteditable='true']");
  var hasModifier = event.ctrlKey || event.metaKey;
  var key = event.key.toLowerCase();
  var buttonId = null;

  if (!isEditable && hasModifier && key === "z") {
    buttonId = event.shiftKey ? "redo_twilight_label" : "undo_twilight_label";
  } else if (!isEditable && event.ctrlKey && key === "y") {
    buttonId = "redo_twilight_label";
  }

  if (buttonId) {
    var button = document.getElementById(buttonId);
    if (button && !button.disabled) {
      event.preventDefault();
      button.click();
    }
  }
});
