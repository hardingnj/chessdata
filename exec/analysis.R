#! /usr/bin/Rscript

# TO DO:
# add capping to scores. Maybe 900? This can be higher than mercy rule??


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

my_games <- get_game_level_dfs(
	FUN = summarize_game,
	limit = NULL
	);

my_games$moves <- do.call(rbind, my_games$moves)

chess <- with(my_games, cbind(games, moves)); 
chess$RES <- factor(c('White', 'Black', 'Draw')[chess$result+1])
chess$RES <- relevel(chess$RES, ref = "Draw")

chess$diff_mean = with(chess, White_mean - Black_mean);
chess$diff_found = with(chess, White_found - Black_found);
chess$diff_blun_3 = with(chess, White_blun_3 - Black_blun_3);

p <- ggplot(chess, aes(Black_found, White_found))
p + geom_point(aes(colour = RES))

test <- multinom(RES ~ White_mean*diff_mean + diff_found, data = chess)
summary(test);
