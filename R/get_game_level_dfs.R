#' Create a game level object from the database
#'
#' This function takes a SQL query (default all): splits the result arrays and calculates 
#' various metrics for black and white. Only processed rows are returned. The function applied to
#' each game is passed in. In most cases this function is expected to be summarize_game.
#'
#' @param sql sql statement to select the rows of interest from the database.
#' @param path path to sqlite database: probably in /inst/extdata/ dir of package
#' @param FUN function to be applied on each game dataframe. "..." as additional arguments.
#' @param limit maximum number of rows to return. Set to NULL for unlimited. 
#' @export
#' @import reshape2 RSQLite futile.logger

get_game_level_dfs <- function(
    sql   = "select games.*, p1.given_name as white_given_name, p1.surname as white_surname, 
             p2.given_name as black_given_name, p2.surname as black_surname from games 
             join players p1 on games.white=p1.pid join players p2 on games.black=p2.pid
             where processed=1",
    path  = system.file(file.path('inst', 'extdata', 'chessAnalysis.db'), package='chessdata'),
    FUN   = NULL,
    limit = 1000,
    ...
    ){

  stopifnot(file.exists(path));
  if(!is.null(limit)) sql <- paste(sql, "limit", limit);

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
  chess.data   <- dbGetQuery(con, sql);
  flog.info("Query has returned %s rows", nrow(chess.data));

  # This is a list of chunk.size lists of 8 vectors. Maybe use stringr here to benchmark
  flog.warn("currently read all char matrices into memory: could be more efficient here");
  ml.data.list <- apply(chess.data[, array.names], 1, strsplit, ',')

  # now remove these columns from game data
  chess.data <- chess.data[, !colnames(chess.data) %in% array.names]

  flog.info("Beginning cast to dataframe");
  time.to.cast <- system.time({
    move.level.data <- lapply(
      ml.data.list,
      function(x) {
        #x is a list of char vectors:
        suppressWarnings({
          single.game.data <- data.frame(
              x[1:4],
              lapply(x[5:8], as.numeric),
              stringsAsFactors = FALSE
              );
        });

        single.game.data <- annotate_game(single.game.data);

        if(!is.null(FUN)) { 
          single.game.data <- FUN(single.game.data, ...);
          }
        return(single.game.data);
      });
  });
  flog.info("Cast timing: ", time.to.cast, capture = TRUE);

  # this is now a list of games, with one row per move.
  names(move.level.data) <- paste0('G', chess.data$id);

  stopifnot(length(move.level.data) == nrow(chess.data));

  return(
    list(
      games = chess.data, 
      moves = move.level.data
    )
  );
}
