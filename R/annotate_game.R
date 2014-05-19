annotate_game <- function(game_to_annotate) { 
  
  moveNo <- as.integer((1 + 1:nrow(game_to_annotate))/2); 
  whoToMove <- ifelse(duplicated(moveNo), 'Black', 'White'); 
  # overwrite based on mate scores
  # might move to game_level_data.
  comb_move_scores <-  with(
    game_to_annotate, 
    ifelse(is.na(move_scores), 
    500*sign(move_mate_in), 
    move_scores)
    );

  comb_opt_move_scores <-  with(
    game_to_annotate, 
    ifelse(is.na(opt_move_scores), 
    500*sign(opt_move_mate_in), 
    opt_move_scores)
    );
  # should go in main?
  score_diff <- comb_opt_move_scores - comb_move_scores;

  cbind(
	  game_to_annotate,
	  moveNo,
	  whoToMove,
	  comb_move_scores,
	  comb_opt_move_scores,
	  score_diff
	);
}
