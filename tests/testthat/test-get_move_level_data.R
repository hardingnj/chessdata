context("Test get_game_level_dfs");

my_games <- get_game_level_dfs(limit=500);

test_that("get_game_level_dfs works when returning raw dataframes", {
  expect_is(my_games, 'list');
  expect_true(all(lapply(my_games$moves, ncol) == 13));
  expect_true(all(lapply(my_games$moves, class) == 'data.frame'));
  });

my_games <- get_game_level_dfs(FUN = summarize_game, limit = 1000);

test_that("Summarize game works as expected and returns list of numeric vectors", {
  expect_is(my_games, 'list');
  expect_true(all(lapply(my_games$moves, length) == 12));
  expect_true(all(lapply(my_games$moves, class) == 'numeric'));
  })
