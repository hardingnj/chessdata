# Trivial function to return the proportion of items in a numeric vector that exceed a value. 
score_prop <- function( x, value=300, inv = FALSE) { 
  x <- na.omit(x); 
  s <- sum(x > value); 
  if(inv) 1 - s/length(x) else s/length(x); 
} 

