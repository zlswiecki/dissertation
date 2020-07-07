plot.group.outs = function (enaplot, points = NULL, method = "mean", labels = NULL,
          colors = default.colors[1], shape = c("square", "triangle-up",
                                                "diamond", "circle"), confidence.interval = c("none",
                                                                                              "crosshairs", "box"), outlier.interval = c("none", "crosshairs",
                                                                                                                                         "box"), label.offset = "bottom right", label.font.size = NULL,
          label.font.color = NULL, label.font.family = NULL, show.legend = T,
          legend.name = NULL, ...)
{
  shape = match.arg(shape)
  confidence.interval = match.arg(confidence.interval)
  outlier.interval = match.arg(outlier.interval)
  if (is.null(points)) {
    stop("Points must be provided.")
  }
  else if (is(points, "ena.points")) {
    points = remove_meta_data(points)
  }
  if (confidence.interval == "crosshairs" && outlier.interval ==
      "crosshairs") {
    print("Confidence Interval and Outlier Interval cannot both be crosshair")
    print("Plotting Outlier Interval as box")
    outlier.interval = "box"
  }
  confidence.interval.values = NULL
  outlier.interval.values = NULL
  if ((is(points, "data.frame") || is(points, "matrix")) &&
      nrow(points) > 1) {
    if (is.null(method) || method == "mean") {
      if (confidence.interval != "none") {
        confidence.interval.values = matrix(c(as.vector(t.test(points[,
                                                                      1], conf.level = 0.95)$conf.int), as.vector(t.test(points[,
                                                                                                                                2], conf.level = 0.95)$conf.int)), ncol = 2)
      }
      if (outlier.interval != "none") {
        iqrs = c(IQR(points[, 1]),IQR(points[, 2])) * 1.5
        q1.x = quantile(points[,1],1/4)
        q3.x = quantile(points[,1],3/4)
        q1.y = quantile(points[,2],1/4)
        q3.y = quantile(points[,2],3/4)

        out.int.vals.x = c(q1.x - iqrs[1],q3.x + iqrs[1])
        out.int.vals.y = c(q1.y - iqrs[2],q3.y + iqrs[2])
        outlier.interval.values = cbind(out.int.vals.x,out.int.vals.y)
      }
      if (length(unique(colors)) > 1) {
        points = t(sapply(unique(colors), function(color) colMeans(points[color ==
                                                                            colors, ]), simplify = T))
        colors = unique(colors)
        attr(enaplot, "means") <- length(attr(enaplot,
                                              "means")) + length(colors)
      }
      else {
        points = colMeans(points)
        attr(enaplot, "means") <- length(attr(enaplot,
                                              "means")) + 1
      }
    }
    else {
      if (confidence.interval != "none")
        warning("Confidence Intervals can only be used when method=`mean`")
      if (outlier.interval != "none")
        warning("Outlier Intervals can only be used when method=`mean`")
      points = apply(points, 2, function(x) do.call(method,
                                                    list(x)))
      attr(enaplot, "means") <- length(attr(enaplot, "means")) +
        1
    }
  }
  enaplot %<>% ena.plot.points(points = points, labels = labels,
                               colors = colors, shape = shape, confidence.interval = confidence.interval,
                               confidence.interval.values = confidence.interval.values,
                               outlier.interval = outlier.interval, outlier.interval.values = outlier.interval.values,
                               label.offset = label.offset, label.font.size = label.font.size,
                               label.font.color = label.font.color, label.font.family = label.font.family,
                               show.legend = show.legend, legend.name = legend.name,
                               ...)
  return(enaplot)
}
