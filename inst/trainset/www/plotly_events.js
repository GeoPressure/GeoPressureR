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

      // Only send if range is explicitly present (or autorange).
      if (!autorange && (xmin == null || xmax == null)) {
        return;
      }

      Shiny.setInputValue(
        "plotly_relayout_xrange",
        {
          xmin: xmin,
          xmax: xmax,
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

    var selectionRange = null;
    if (eventData && eventData.range && Array.isArray(eventData.range.x)) {
      selectionRange = {
        xmin: eventData.range.x[0],
        xmax: eventData.range.x[1],
        ymin: eventData.range.y ? Number(eventData.range.y[0]) : null,
        ymax: eventData.range.y ? Number(eventData.range.y[1]) : null,
        y2min: eventData.range.y2 ? Number(eventData.range.y2[0]) : null,
        y2max: eventData.range.y2 ? Number(eventData.range.y2[1]) : null,
      };
    }

    // Send combined event data with key state
    Shiny.setInputValue(
      "plotly_selected_with_keys",
      {
        points: cleanEventData,
        range: selectionRange,
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
