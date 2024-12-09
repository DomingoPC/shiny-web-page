Shiny.addCustomMessageHandler("switchTheme", function(params) {
    var link = document.querySelector('link[rel=stylesheet][href*="shinythemes"]');
    if (link) {
      var newHref = link.href.replace(/\/(.*).css$/, "/" + params.theme + ".css");
      link.href = newHref;
    }

  });