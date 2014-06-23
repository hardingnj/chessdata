annotate_game <- function(game_to_annotate, mate_in_value = 900) { 

  moveNo <- as.integer((1 + 1:nrow(game_to_annotate))/2); 
  whoToMove <- factor(
    rep_len(c('White', 'Black'), length.out = nrow(game_to_annotate)),
    levels = c('White', 'Black')
  )

  # overwrite based on mate scores
  comb_move_scores <- with(
    game_to_annotate, 
    ifelse(
      is.na(move_scores), 
      mate_in_value * sign(move_mate_in), 
      move_scores 
      )
    );

  comb_opt_move_scores <- with(
    game_to_annotate, 
    ifelse(
      is.na(opt_move_scores), 
      mate_in_value * sign(opt_move_mate_in), 
      opt_move_scores
      )
    );

  # cap at mate_in_value
  comb_opt_move_scores <- pmin(abs(comb_opt_move_scores), mate_in_value) * sign(comb_opt_move_scores);
  comb_move_scores     <- pmin(abs(comb_move_scores),     mate_in_value) * sign(comb_move_scores);
 
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
