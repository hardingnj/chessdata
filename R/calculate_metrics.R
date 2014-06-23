#' Take a game object and caclulate summary statistics.
#'
#' This function takes a dataframe representing a game and calculates some summary statistics. 
#'
#' @param game_to_eval a dataframe representing a game
#' @param mercy_rule a logical vector representing whether or not to mask each move 
#' @export

calculate_metrics <- function(game_to_eval, mercy_rule = rep(TRUE, nrow(game_to_eval))){

  # This is required in the possible event that whoToMove only has a single level.
  stopifnot(game_to_eval$whoToMove[1] == 'White');
  game_to_eval <- game_to_eval[mercy_rule,]

  spl_game <- split(
    game_to_eval,
    game_to_eval$whoToMove
  );

  metrics <- lapply(
    spl_game,
    function(g) {
      with(g, list(
        mean   = mean(score_diff, na.rm=T),
        blun_1 = score_prop(score_diff, value = 100),
        blun_3 = score_prop(score_diff, value = 300),
        blun_5 = score_prop(score_diff, value = 500),
        found  = score_prop(score_diff, value = 0, inv = TRUE)
      ))
     });

  out <- c(
    nMovesTot = length(mercy_rule),
    nMoves_MR = sum(mercy_rule),
    unlist(metrics)
  );
  stopifnot(length(out) == 12)
  return(out);
}
