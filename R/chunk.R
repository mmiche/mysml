# chunk
#
#
chunk <- function(x, n) {split(x, cut(seq_along(x), n, labels = FALSE))}
# https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
# Answer by mathheadinclouds (Apr 29, 2013 at 9:37)
