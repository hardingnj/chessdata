calculate_metrics <- function(game_to_eval){
  score_prop <- function(
	  x,
	  lim=300,
	  inv = FALSE
	) { x <- na.omit(x); s <- sum(x > lim); if(inv) 1 - s/length(x) else s/length(x); } 

  with(
    game_to_eval,
    list(
      mean   = tapply(score_diff, whoToMove, mean, na.rm=T),
      blun_1 = tapply(score_diff, whoToMove, score_prop, lim = 100),
      blun_3 = tapply(score_diff, whoToMove, score_prop, lim = 300),
      blun_5 = tapply(score_diff, whoToMove, score_prop, lim = 500),
      found  = tapply(score_diff, whoToMove, score_prop, lim = 0, inv = TRUE)
    ));
}
