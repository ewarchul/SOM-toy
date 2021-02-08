require(kohonen)

read_data <- function (data_path) {
  data_files <- list.files(path=data_path, pattern = "*.txt")
  data_files <- paste(data_path, data_files, sep = "/")
  data <- do.call(rbind, lapply(data_files, read.table, header = F, sep = "\t", col.names = c("d", "p1", "p2")))
}

data_path <- "data"
results_path <- "results"
data <- read_data(data_path)
data <- scale(data)

x_neurons <- c(2, 3, 5)
y_neurons <- c(10, 20, 30)
distances <- c("euclidean", "manhattan", "sumofsquares")
radius <- c(NA, 1)
topologies <- c("rectangular", "hexagonal")
neighbourhood <- c("bubble", "gaussian")
toroidal <- c(T, F)

dir.create(results_path, showWarnings = F)
all_exp_cnt <- length(x_neurons) * length(y_neurons) * length(distances) * length(radius) * length(topologies) * length(neighbourhood) * length(toroidal)
iter <- 0

for (xn in x_neurons)
  for (yn in y_neurons)
    for (dist in distances)
      for (r in radius)
        for (top in topologies)
          for (neigh in neighbourhood)
            for (tor in toroidal) {
              iter <- iter + 1
              print(sprintf("Experiment %d/%d", iter, all_exp_cnt))
              if (xn * yn > nrow(data)) {
                print("Skipping configuration - too much neurons!")
                next
              }
              if (is.na(r)) {
                som <- som(data,
                           grid=somgrid(xdim = xn,
                                        ydim = yn,
                                        topo = top,
                                        neighbourhood.fct = neigh,
                                        toroidal = tor),
                           rlen = 1000,
                           alpha = c(0.05, 0.001),
                           dist.fcts = dist,
                           mode = "pbatch",
                           cores = -1)
              }
              else {
                som <- som(data,
                           grid=somgrid(xdim = xn,
                                        ydim = yn,
                                        topo = top,
                                        neighbourhood.fct = neigh,
                                        toroidal = tor),
                           rlen = 1000,
                           alpha = c(0.1, 0.001),
                           dist.fcts = dist,
                           radius = r,
                           mode = "pbatch",
                           cores = -1)
              }
              print(sprintf("Saving to %s/%s_%s_%s_%s_%s_%s_%s.rds", results_path, xn, yn, dist, r, top, neigh, tor))
              saveRDS(som, file = sprintf("%s/%s_%s_%s_%s_%s_%s_%s.rds", results_path, xn, yn, dist, r, top, neigh, tor))
            }