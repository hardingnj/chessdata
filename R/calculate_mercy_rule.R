calculate_mercy_rule <- function(game, mercy_rule_limit = 500){

  # mercy rule "masks" all moves from a point where someone goes > X cp down.
  cut_off <- match(
    TRUE,
    abs(game$comb_move_scores) > mercy_rule_limit,
    nomatch = nrow(game) + 1
  );
  if(cut_off %% 2 == 1) cut_off <- cut_off + 1; 
  return(1:nrow(game) <= cut_off);
}
