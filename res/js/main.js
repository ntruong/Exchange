const arrow = "<svg viewBox=\"0 0 1024 512\"><path d=\"M0 0 L512 512 L1024 0 L824 0 L512 312 L200 0Z\"/></svg>";

const bookmarks = [
  {
    red: "http://redacted.ch/login.php",
    git: "https://github.com/login",
    ucf: "https://texasucf.slack.com",
    mail: "https://inbox.google.com"
  },
  {
    canvas: "https://canvas.utexas.edu/",
    piazza: "https://www.piazza.com/",
    ocr: "https://msb-utexas-csm.symplicity.com/",
    drive: "https://drive.google.com/"
  }
];

function $(id) { return document.getElementById(id); };

// Create a list of lists of links to create a bookmark menu
function buildBookmarks(links) {
  var container = document.createElement("ul");
  container.classList.add("bookmarks");
  links.forEach((dict, idx, ls) => {
    var li = document.createElement("li");
    var col = document.createElement("ul");
    col.classList.add("hiddenV");
    for (var key in dict) {
      col.innerHTML += "<li><a href=\"" + dict[key] + "\">" + key + "</a></li>";
    };
    li.innerHTML += arrow;
    li.appendChild(col);
    container.appendChild(li);
  });
  return container;
};

// Create the appropriate greeting
function greet() {
  var d = new Date();
  var n = d.getHours();
  var msg = 'good ';

  if      ( n >= 22 || n == 0  ) { msg += "night"; }
  else if ( n >= 1  && n <= 4  ) { msg += "night"; }
  else if ( n >= 5  && n <= 11 ) { msg += "morning"; }
  else if ( n >= 12 && n <= 17 ) { msg += "afternoon"; }
  else if ( n >= 18 && n <= 21 ) { msg += "evening"; }

  return "<h1>" + msg + "</h1>";
};

// Hide all children (recursively) of a given node
function obfuscate(node, hide = true) {
  if (hide) { node.style.visibility = "hidden"; };
  Array.prototype.forEach.call(node.children, (x) => obfuscate(x));
};

// Reveal all children (recursively) of a given node
function reveal(node) {
  node.style.visibility = "visible";
  Array.prototype.forEach.call(node.children, reveal);
};

// Focus a given main panel, passed an anchor (nav button) as an argument
function focus(anchor) {
  var panelId = Array.prototype.indexOf.call(anchor.parentNode.children, anchor);

  var active = document.querySelectorAll(".main:not(.hidden)")[0];
  obfuscate(active, hide=false);
  window.setTimeout(() => {
    active
      .classList
      .add("hidden");
    $("navbar").getElementsByClassName("depressed")[0]
      .classList
      .remove("depressed");
  }, 200);

  // Delay so the main panels don't appear at the same time
  var inactive = document.getElementsByClassName("main")[panelId];
  window.setTimeout(() => {
    inactive
      .classList
      .remove("hidden");
    $("navbar").children[panelId]
      .classList
      .add("depressed");
  }, 550);
  window.setTimeout(() => reveal(inactive), 750);
};

// Listen for nav button clicks to focus the appropriate panel
document.addEventListener("click", function(event) {
  if (Array.prototype.indexOf.call($("navbar").children, event.target) != -1) {
    focus(event.target);
  };
});

// Load extras when the document is ready
document.addEventListener("DOMContentLoaded", function() {
  $("bookmarks").appendChild(buildBookmarks(bookmarks));
  $("greeting").innerHTML = greet();
});
