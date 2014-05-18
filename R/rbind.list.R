# FUNCTIONS
rbind.list <- function(x) {
    y <- do.call(rbind, x);
	nrows <-  unlist(lapply(x, function(x) { nrow <- nrow(x) ; if(is.null(nrow)) {nrow <- 0}; nrow; } ))

    row.origin.name <- rep(names(x), times = nrows);
    y <- cbind(y, id = row.origin.name);
    y
    }
