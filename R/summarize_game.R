#' Take a game object and caclulate summary statistics.
#'
#' This function takes a dataframe representing a game and calculates some summary statistics. 
#' Although essentially a wrapper for calculating the 'mercy rule' and calculate_metrics.
#'
#' @param game a dataframe representing a game
#' @param mercy_rule_limit a value representing how far behind a player must get before mercy rule is applied
#' @export

summarize_game <- function(game, mr_limit = 500){

  calculate_metrics(
    game,
    calculate_mercy_rule(game, mr_limit)
  );
}
