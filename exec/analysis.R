#! /usr/bin/Rscript

## TO DO: Add function that allows grabbing of single game id. Prob an overwirte to sql in get_game_level_dfs

# 3 issues
# Some games blitz: filter these out
# Missing dates on several games
# Different name strings.
# add sliding window metric of strength. Add weight of nMoves - 8.

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

row_per_game <- function(){

  my_games <- get_game_level_dfs(
    limit = NULL,
    FUN = summarize_game,
    mr_limit = 1000
    );

  my_games$moves <- do.call(rbind, my_games$moves)
  dat <- with(my_games, cbind(games, moves)); 
  dat$diff_mean   = with(dat, White.mean - Black.mean);
  dat$diff_found  = with(dat, White.found - Black.found);
  dat$diff_blun_3 = with(dat, White.blun_3 - Black.blun_3);
  dat <- dat[order(dat$diff_mean),]
  return(dat);
}

output <- "~/chess_analysis";
chess <- row_per_game();

# find top 5 black results...
subset(chess, result == 'Black')[1:5,]

#ggplot(chess, aes(Black.found, White.found)) + geom_point(aes(colour = RES))
pdf(file.path(output, 'mean_vs_result_scatter.pdf'))
ggplot(chess, aes(Black.mean, White.mean)) + geom_point(aes(colour = result, alpha = nMovesTot)) + xlim(0, 150) + ylim(0, 150) + geom_abline(intercept = 0, slope = 1)
dev.off();

# plot Carlsen/Fisher games only.

# going to have to impute date... 
chess$date <- with(
	chess,
	as.Date(paste(
      year, 
      month, 
      day, 
      sep='/'
    ))
);

# prep melt.
id.vars <- c('id', 'event', 'site', 'white', 'black', 'result', 'date')
measure.vars <- grep('^(Black|White)\\.', colnames(chess), value=T)

test <- chess[,c(id.vars, measure.vars)]
qq<-melt(test,id.vars=id.vars, measure.vars=measure.vars)

dd <- do.call(
  rbind,
  strsplit(
    as.character(qq[,'variable']),
    '\\.'
  )
)
colnames(dd) <- c('player', 'variable');
qq$variable <- NULL;
qq <- cbind(qq, dd);
cas <- dcast(qq, '... ~ variable')

cas$pid <- with(cas, ifelse(player == 'White', white, black));
# And id of opponent
cas$opponent <- with(cas, ifelse(player == 'White', black, white));

cas$black <- NULL;
cas$white <- NULL;

# Get player data... can remove complex sql from get_game_level_dfs
drv <- dbDriver("SQLite")
con <- dbConnect(drv,system.file(file.path('inst', 'extdata', 'chessAnalysis.db'), package='chessdata'))
players   <- dbGetQuery(con, 'select * from players;');
players$full_name <- with(players, paste(given_name, surname))

cas <- merge(cas, players[,c('pid', 'full_name')]);

# to only subset to players with high N.
top.five <- names(sort(table(cas$full_name), TRUE)[1:5])

ggplot(subset(cas, full_name %in% top.five), aes(date, mean, group=full_name,colour=full_name)) + geom_point() + geom_smooth(size=2, se=F)


test <- multinom(result ~ White.mean*diff_mean + diff_found, data = chess)
summary(test);
