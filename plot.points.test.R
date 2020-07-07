plot.points.test = function (enaplot, points = NULL, point.size = enaplot$point$size,
          labels = NULL, label.offset = "top left", label.group = NULL,
          label.font.size = NULL, label.font.color = NULL, label.font.family = NULL,
          shape = "circle", colors = NULL, confidence.interval.values = NULL,
          confidence.interval = c("none", "crosshairs", "box"), outlier.interval.values = NULL,
          outlier.interval = c("none", "crosshairs", "box"), show.legend = T,
          legend.name = "Points", texts = NULL, ...)
{
  env = environment()
  for (n in c("font.size", "font.color", "font.family")) {
    if (is.null(get(paste0("label.", n))))
      env[[paste0("label.", n)]] = enaplot$get(n)
  }
  if (is.null(points)) {
    points = enaplot$enaset$points
  }
  if (is(points, "numeric")) {
    points = matrix(points)
    dim(points) = c(1, nrow(points))
    points.layout = data.table::data.table(points)
  }
  else if (is.data.table(points)) {
    points.layout = data.table::copy(points)
  }
  else {
    points.layout = data.table::data.table(points)
  }
  if (!is.character(label.font.family)) {
    label.font.family = enaplot$get("font.family")
  }
  confidence.interval = match.arg(confidence.interval)
  outlier.interval = match.arg(outlier.interval)
  valid.shapes = c("circle", "square", "triangle-up", "diamond")
  if (!all(shape %in% valid.shapes))
    stop(sprintf("Unrecognized shapes: %s", paste(unique(shape[!(shape %in%
                                                                   valid.shapes)]), collapse = ", ")))
  if (length(shape) == 1)
    shape = rep(shape, nrow(points.layout))
  valid.label.offsets = c("top left", "top center", "top right",
                          "middle left", "middle center", "middle right", "bottom left",
                          "bottom center", "bottom right")
  if (!all(label.offset %in% valid.label.offsets))
    stop(sprintf("Unrecognized label.offsets: %s", paste(unique(label.offset[!(label.offset %in%
                                                                                 valid.label.offsets)]), collapse = ", ")))
  if (length(label.offset) == 1)
    label.offset = rep(label.offset, nrow(points.layout))
  if (grepl("^c", confidence.interval) && grepl("^c", outlier.interval)) {
    print("Confidence Interval and Outlier Interval cannot both be crosshair")
    print("Plotting Outlier Interval as box")
    outlier.interval = "box"
  }
  if (length(colors) == 1) {
    colors = rep(colors, nrow(points.layout))
  }
  if (length(point.size) == 1)
    point.size = rep(point.size, nrow(points.layout))
  if (is.null(labels))
    show.legend = F
  error = list(x = list(visible = T, type = "data"), y = list(visible = T,
                                                              type = "data"))
  int.values = NULL
  if (grepl("^c", confidence.interval) && !is.null(confidence.interval.values)) {
    int.values = confidence.interval.values
  }
  else if (grepl("^c", outlier.interval) && !is.null(outlier.interval.values)) {
    int.values = outlier.interval.values
  }
  error$x$array = int.values[, 1]
  error$y$array = int.values[, 2]
  box.values = NULL
  if (grepl("^b", confidence.interval) && !is.null(confidence.interval.values)) {
    box.values = confidence.interval.values
    box.label = "Conf. Int."
  }
  if (grepl("^b", outlier.interval) && !is.null(outlier.interval.values)) {
    box.values = outlier.interval.values
    box.label = "Outlier Int."
  }
  points.matrix = remove_meta_data(points.layout)
  colnames(points.matrix) = paste0("X", rep(1:ncol(points.matrix)))
  this.max = max(points.matrix)
  for (m in 1:nrow(points.matrix)) {
    enaplot$plot = plotly::add_trace(p = enaplot$plot, data = points.matrix[m,
                                                                            ], type = "scatter", x = ~X1, y = ~X2, mode = "markers+text",
                                     marker = list(symbol = shape[m], color = colors[m],
                                                   size = point.size[m]), error_x = error$x, error_y = error$y,
                                     showlegend = show.legend, name = labels[m], text = texts[m],
                                     textfont = list(family = label.font.family, size = label.font.size,
                                                     color = label.font.color), legendgroup = legend.name,
                                     textposition = label.offset[m], hoverinfo = "x+y+name")
  }
  if (!is.null(box.values)) {
    browser()

    boxv = data.frame(
      X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1] ,box.values[1,1]),
      X2 = c(box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2], box.values[1,2])
    )
    this.max = max(boxv, this.max)
    enaplot$plot = plotly::add_trace(p = enaplot$plot, data = boxv,
                                     type = "scatter", x = ~X1, y = ~X2, mode = "lines",
                                     line = list(width = 1, color = colors[1], dash = "dash"),
                                     showlegend = show.legend, name = box.label)
  }
  if (this.max * 1.2 > max(enaplot$axes$y$range)) {
    this.max = this.max * 1.2
    enaplot$axes$x$range = c(-this.max, this.max)
    enaplot$axes$y$range = c(-this.max, this.max)
    enaplot$plot = plotly::layout(enaplot$plot, xaxis = enaplot$axes$x,
                                  yaxis = enaplot$axes$y)
  }
  return(enaplot)
}
