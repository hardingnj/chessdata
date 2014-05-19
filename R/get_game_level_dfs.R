#' Create a game level object from the database
#'
#' This function takes a SQL query (default all): splits the result arrays and calculates 
#' various metrics for black and white.
#'
#' @param sql sql statement to select the rows of interest from the database.
#' @param path path to sqlite database: probably in /data of package
#' @param chunk_size number of rows to return at a time, essentially memory/speed trade off.
#' @export
#' @import reshape2 RSQLite futile.logger

get_game_level_dfs <- function(
    sql  = "select * from games where id < 500;",
    path = system.file(file.path('inst', 'extdata', 'chessAnalysis.db'), package='chessdata')
    ){
  require(RSQLite);
  stopifnot(file.exists(path));

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
  chess.data   <- dbGetQuery(
    con,
    sql
  );
  flog.info("Query has returned %s rows", nrow(chess.data));

  # This is a list of chunk.size lists of 8 vectors. Maybe use stringr here to benchmark
  ml.data.list <- apply(chess.data[, array.names], 1, strsplit, ',')

  # now remove these columns from game data
  chess.data <- chess.data[, !colnames(chess.data) %in% array.names]

  flog.info("Beginning cast to dataframe");
  time.to.cast <- system.time({
    move.level.data <- lapply(
      ml.data.list,
      function(x) {
        #x is a list of char vectors:
        data.frame(
			x[1:4],
			lapply(x[5:8], as.numeric),
            stringsAsFactors = FALSE
			);
      });
  });
  flog.info("Cast timing: ", time.to.cast, capture = TRUE);

  flog.info("Calculating move numbers and white/black");
    move.level.data <- lapply(
      move.level.data,
      annotate_game
    );

  flog.info("Adding id column");
  move.level.data <- lapply(
    names(move.level.data),
    function(id) { cbind(id, move.level.data[[id]]) }
    );

  # this is now a list of games, with one row per move.
  names(move.level.data) <- paste0('G', 1:length(move.level.data));
  return(list(games = chess.data, moves = move.level.data));

  # 2 things needed: add ability to process each move list separately. Allow passing in of function to do this.
  # Fix so that we know ids match across moves/games.
}
