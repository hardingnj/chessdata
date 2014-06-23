context("Test calculate_metrics");

my_games <- get_game_level_dfs(limit=1);

metrics <- calculate_metrics(my_games$moves[[1]])

test_that("Calculate metrics returns expected object", {
  expect_is(metrics, 'numeric');
  expect_true(length(metrics) == 12);
  })

summarize_game(my_games$moves[[1]], mr_limit = 500);

