#' Take a game object and caclulate summary statistics.
#'
#' This function takes a dataframe representing a game and calculates some summary statistics. 
#'
#' @param game a dataframe representing a game
#' @param mercy_rule_limit a value representing how far behind a player must get before mercy rule is applied
#' @export
#' @import reshape2 

calculate_metrics <- function(game_to_eval, mercy_rule){

  # This is required in the possible event that whoToMove only has a single level.
  levels(game_to_eval$whoToMove) <- c('White', 'Black')

  metrics <- with(
    subset(game_to_eval, mercy_rule),
    list(
      mean   = tapply(X = score_diff, INDEX = whoToMove, FUN = mean, na.rm=T),
      blun_1 = tapply(X = score_diff, INDEX = whoToMove, FUN = score_prop, value = 100),
      blun_3 = tapply(X = score_diff, INDEX = whoToMove, FUN = score_prop, value = 300),
      blun_5 = tapply(X = score_diff, INDEX = whoToMove, FUN = score_prop, value = 500),
      found  = tapply(X = score_diff, INDEX = whoToMove, FUN = score_prop, value = 0, inv = TRUE)
    ));
 
  mm <- melt(do.call(rbind, metrics))
  cast <- dcast(mm, ". ~ Var2 + Var1")[,-1]

  out <- cbind(nMovesTot = nrow(game_to_eval), nMoves_MR = sum(mercy_rule), cast);
  stopifnot(ncol(out) == 12)
  return(out);
}
