#' Create a game level object from the database
#'
#' This function takes a SQL query (default all): splits the result arrays and calculates 
#' various metrics for black and white.
#'
#' @param sql sql statement to select the rows of interest from the database.
#' @param path path to sqlite database: probably in /data of package
#' @param chunk_size number of rows to return at a time, essentially memory/speed trade off.
#' @export
#' @import reshape2 RSQLite

create_tables_from_db <- function(
    sql  = "select * from games",
	path = file.path(Sys.getenv('HOME'), 'chessDB', 'chessAnalysis.db'),
	chunk_size = 1000
	){

  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, path)

  # These are the 'per game moves'
  array.names <- c(
    "algebraic_moves",
    "opt_algebraic_moves",
    "coordinate_moves",
    "opt_coordinate_moves",
    "move_scores",
    "opt_move_scores",
    "move_mate_in",
    "opt_move_mate_in"
  );

  # Query to get all game data
  id_range <- c(0, chunk.size);
  repeat {
    query_string <- paste("Select * from games where id > ", id_range[1], ", id <", id_range[1]); 
    chess.data   <- dbGetQuery(
		con,
		query_string
		);
    if(is.null(chess.data)) break;
    # To do: result as factor.
    #      : White/black as factor

    # Improve casting/creation of dataframes.

    # This is a list of chunk.size lists of 8 vectors.
    aa <- apply(chess.data[, array.names], 1, strsplit, ',')

    # now remove from game data
    game.data <- chess.data[, !colnames(chess.data) %in% array.names]

    chess.data <- NULL; # save memory

    # qq is the key object: from this we need to add a vector that represents black/white.
    qq <- lapply(
      aa,
      do.call,
      what=cbind
    );
    aa <- NULL;

    qq <- lapply(
	  qq,
	  function(x) {
        # x is a matrix, and I want to convert some columns  Currently is char matrix
        dat <- as.data.frame(x, stringsAsFactors = FALSE);
        dat[, array.names[5:8]] <- sapply(dat[, array.names[5:8]], as.numeric)
        dat;
      });

    qq <- lapply(
      qq,
      function(x) { moveNo <- as.integer((1 + 1:nrow(x))/2); whiteMove <- !duplicated(moveNo); cbind(x, moveNo, whiteMove); }
    );

    # now each of these items should be bound to their ID, which is their index.
    move.data <- rbind.list(qq);
    qq <- NULL;

    # here we need to calculate differences
    move.data[,'score_diff'] <- with(move.data, move_scores  - opt_move_scores);
    move.data[,'mate_diff']  <- with(move.data, move_mate_in - opt_move_mate_in);

    # other analysis metrics?

    # here write both to file.
  }
  return(TRUE);
}
# End goals: 
# is one df with two rows per game, one black, one white. that has mean, median, etc of (move-opt). Or black_mean, white_mean etc
# One df with one row per move. 
# [1] "id"                   "event"                "site"                
# [4] "fileid"               "white"                "black"               
# [7] "result"               "year"                 "month"               
#[10] "day"                  "round"                "whiteELO"            
#[13] "blackELO"             "ECO"                  "algebraic_moves"     
#[16] "coordinate_moves"     "move_scores"          "move_mate_in"        
#[19] "opt_algebraic_moves"  "opt_coordinate_moves" "opt_move_scores"     
#[22] "opt_move_mate_in"     "processed"            "time_s"  




