gen_categorical <- function(N, vec_distr) {
  cut(runif(N, 0, 1),
    breaks = c(0,cumsum(vec_distr)),
    labels = 1:length(vec_distr),
    right  = FALSE
)}