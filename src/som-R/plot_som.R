require(kohonen)

results_path <- "results"
graphics_path <- "img"
dir.create(graphics_path, showWarnings = F)

x_neurons <- c(2, 3, 5)
y_neurons <- c(10, 20, 30)
distances <- c("sumofsquares", "euclidean", "manhattan")
radius <- c(NA, 1)
topologies <- c("rectangular", "hexagonal")
neighbourhood <- c("gaussian", "bubble")
toroidal <- c(F, T)
plot_types <- c("change", "counts")

for (xn in x_neurons)
  for (yn in y_neurons)
    for (dist in distances)
      for (r in radius)
        for (top in topologies)
          for (neigh in neighbourhood)
            for (tor in toroidal)
              for (pt in plot_types) {
                file_fmt <- "%s_%s_%s_%s_%s_%s_%s"
                file_template <- sprintf(file_fmt, xn, yn, dist, r, top, neigh, tor)
                som <- readRDS(paste0(results_path, "/", file_template, ".rds"))

                plot_file <- paste0(graphics_path, "/", file_template, "_", pt, ".eps")
                setEPS()
                postscript(plot_file)
                plot(som, type = pt, shape = "straight")
                dev.off()
              }
