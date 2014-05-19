context("Test get_game_level_dfs");

my_games <- get_game_level_dfs();

test_that("get_game_level_dfs works as expected", {
  expect_is(my_games, 'list');
  expect_true(all(lapply(my_games, ncol) == 10));
  expect_true(all(lapply(my_games, class) == 'data.frame'));
  });
