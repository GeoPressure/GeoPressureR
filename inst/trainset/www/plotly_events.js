/**
 * Plotly Event Handlers for Trainset Application
 * Handles selection and click events with Ctrl/Cmd key detection for label clearing
 */

function setupPlotlyEventHandlers(el) {
  // Debounced relayout handler to update visible x-range (used for windowed rendering)
  var relayoutTimer = null;
  el.on("plotly_relayout", function (eventData) {
    if (relayoutTimer) {
      clearTimeout(relayoutTimer);
    }

    relayoutTimer = setTimeout(function () {
      function toMs(x) {
        if (x == null) return null;
        if (typeof x === "number") return x;
        if (x instanceof Date) return x.getTime();
        if (typeof x === "string") {
          // Plotly sometimes sends numeric values as strings; treat as epoch ms.
          if (/^[0-9]+(\\.[0-9]+)?$/.test(x)) return Number(x);
          var t = new Date(x).getTime();
          return isNaN(t) ? null : t;
        }
        try {
          var t2 = new Date(x).getTime();
          return isNaN(t2) ? null : t2;
        } catch (e) {
          return null;
        }
      }

      var xmin = null;
      var xmax = null;
      var autorange = false;

      if (eventData) {
        autorange = eventData["xaxis.autorange"] === true;
        if (eventData["xaxis.range[0]"] != null && eventData["xaxis.range[1]"] != null) {
          xmin = eventData["xaxis.range[0]"];
          xmax = eventData["xaxis.range[1]"];
        } else if (Array.isArray(eventData["xaxis.range"]) && eventData["xaxis.range"].length >= 2) {
          xmin = eventData["xaxis.range"][0];
          xmax = eventData["xaxis.range"][1];
        }
      }

      // Convert to epoch ms for stable parsing in R
      var xminMs = toMs(xmin);
      var xmaxMs = toMs(xmax);

      // Only send if range is explicitly present (or autorange).
      if (!autorange && (xminMs == null || xmaxMs == null)) {
        return;
      }

      Shiny.setInputValue(
        "plotly_relayout_xrange",
        {
          xmin_ms: xminMs,
          xmax_ms: xmaxMs,
          autorange: autorange,
          nav: window.trainsetLastNav || null,
          timestamp: Date.now(),
        },
        { priority: "event" }
      );

      // Reset after capturing one event
      window.trainsetLastNav = null;
    }, 125);
  });

  // Send ctrl state when selection occurs - capture at moment of event
  el.on("plotly_selected", function (eventData) {
    var ctrlPressed =
      window.trainsetKeyState && window.trainsetKeyState.ctrlOrMeta ? true : false;

    // Extract only needed data to avoid circular references
    var cleanEventData = null;
    if (eventData && eventData.points && Array.isArray(eventData.points)) {
      cleanEventData = eventData.points.map(function (point) {
        return {
          pointNumber: point.pointNumber != null ? point.pointNumber : 0,
          curveNumber: point.curveNumber != null ? point.curveNumber : 0,
          x: point.x != null ? point.x : null,
          y: point.y != null ? point.y : null,
          customdata: point.customdata != null ? point.customdata : null,
        };
      });
    }

    // Send combined event data with key state
    Shiny.setInputValue(
      "plotly_selected_with_keys",
      {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now(),
      },
      { priority: "event" }
    );
  });

  // Also handle click events
  el.on("plotly_click", function (eventData) {
    var ctrlPressed =
      window.trainsetKeyState && window.trainsetKeyState.ctrlOrMeta ? true : false;

    // Extract only needed data to avoid circular references
    var cleanEventData = null;
    if (
      eventData &&
      eventData.points &&
      Array.isArray(eventData.points) &&
      eventData.points.length > 0
    ) {
      var point = eventData.points[0];
      cleanEventData = [
        {
          pointNumber: point.pointNumber != null ? point.pointNumber : 0,
          curveNumber: point.curveNumber != null ? point.curveNumber : 0,
          x: point.x != null ? point.x : null,
          y: point.y != null ? point.y : null,
          customdata: point.customdata != null ? point.customdata : null,
        },
      ];
    }

    // Send combined event data with key state
    Shiny.setInputValue(
      "plotly_click_with_keys",
      {
        points: cleanEventData,
        ctrlPressed: ctrlPressed,
        timestamp: Date.now(),
      },
      { priority: "event" }
    );
  });
}
