summarize_game <- function(game, mercy_rule_limit = 500){

  stopifnot(
    class(game) == 'data.frame'
    );

  # mercy rule "masks" all moves from a point where someone goes > X cp down.
  cut_off <- match(
    TRUE,
    abs(game$comb_move_scores) > mercy_rule_limit,
    nomatch = length(game$comb_move_scores) + 1
  );
  if(cut_off %% 2 == 1) cut_off <- cut_off + 1; 

  game$mercy_rule <- 1:length(game$comb_move_scores) <= cut_off;

  metrics <- calculate_metrics(subset(game, mercy_rule));

  mm <- melt(do.call(rbind, metrics))
  cast <- dcast(mm, ". ~ Var2 + Var1")[,-1]

  cbind(nMoves = nrow(game), cast);
}
