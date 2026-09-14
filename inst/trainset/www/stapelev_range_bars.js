function stapelevRangeBarMs(value) {
  if (value instanceof Date) return value.getTime();
  if (typeof value === "number") {
    if (value < 1e6) return value * 86400000;
    if (value < 1e11) return value * 1000;
    return value;
  }
  if (/^\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}/.test(value) && !/(Z|[+-]\d{2}:?\d{2})$/.test(value)) {
    value = value.replace(" ", "T") + "Z";
  }
  var time = new Date(value).getTime();
  return isNaN(time) ? null : time;
}

function setupStapelevRangeBars(plot) {
  if (plot._stapelevRangeSelectionHandler && plot.removeListener) {
    plot.removeListener("plotly_selected", plot._stapelevRangeSelectionHandler);
  }
  plot._stapelevRangeSelectionHandler = function (eventData) {
    if (!window.stapelevRangeLevel || !eventData || !eventData.range || !eventData.range.x) return;
    var range = eventData.range.x;
    Shiny.setInputValue(
      "stapelev_range_selection",
      {
        level: window.stapelevRangeLevel,
        start_ms: stapelevRangeBarMs(range[0]),
        end_ms: stapelevRangeBarMs(range[1]),
        timestamp: Date.now(),
      },
      { priority: "event" }
    );
    window.stapelevRangeLevel = null;
    $(".stapelev-range-btn").removeClass("active");
    Plotly.restyle(plot, { selectedpoints: [null] });
    Plotly.relayout(plot, { dragmode: "zoom", selections: [] });
  };
  plot.on("plotly_selected", plot._stapelevRangeSelectionHandler);

  if (plot._stapelevViewHandler && plot.removeListener) {
    plot.removeListener("plotly_relayout", plot._stapelevViewHandler);
  }
  window.stapelevPlotViews = window.stapelevPlotViews || {};
  var meta = (plot.layout && plot.layout.meta) || {};
  var viewKey = [meta.stapelev_key, meta.stapelev_view].join("-");
  plot._stapelevViewHandler = function (eventData) {
    var view = window.stapelevPlotViews[viewKey] || {};
    if (eventData["xaxis.autorange"]) delete view.x;
    if (eventData["yaxis.autorange"]) delete view.y;
    if (eventData["xaxis.range"]) view.x = eventData["xaxis.range"];
    if (eventData["yaxis.range"]) view.y = eventData["yaxis.range"];
    if (eventData["xaxis.range[0]"] !== undefined && eventData["xaxis.range[1]"] !== undefined) {
      view.x = [eventData["xaxis.range[0]"], eventData["xaxis.range[1]"]];
    }
    if (eventData["yaxis.range[0]"] !== undefined && eventData["yaxis.range[1]"] !== undefined) {
      view.y = [eventData["yaxis.range[0]"], eventData["yaxis.range[1]"]];
    }
    window.stapelevPlotViews[viewKey] = view;
  };
  plot.on("plotly_relayout", plot._stapelevViewHandler);

  var view = window.stapelevPlotViews[viewKey];
  if (view && (view.x || view.y)) {
    var update = {};
    if (view.x) update["xaxis.range"] = view.x;
    if (view.y) update["yaxis.range"] = view.y;
    Plotly.relayout(plot, update);
  }
}

$(document).ready(function () {
  Shiny.addCustomMessageHandler("stapelevRangeSelect", function (level) {
    var plot = document.getElementById("stapelev_proposal_plot");
    if (!plot) return;
    window.stapelevRangeLevel = Number(level);
    $(".stapelev-range-btn").removeClass("active");
    $("#stapelev_range_btn_" + level).addClass("active");
    Plotly.relayout(plot, { dragmode: "select", selections: [] });
  });
});
