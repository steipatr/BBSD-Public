# Simulate observations from two classes
set.seed(123)
obs <- seqdef(rbind(
  matrix(sample(letters[1:3], 5000, TRUE, prob = c(0.1, 0.6, 0.3)), 500, 10),
  matrix(sample(letters[1:3], 2000, TRUE, prob = c(0.4, 0.4, 0.2)), 200, 10)))
# Initialize the model
set.seed(9087)
demo_model <- build_lcm(obs, n_clusters = 2)
# Estimate model parameters
demo_fit <- fit_model(demo_model)
# How many of the observations were correctly classified:
sum(summary(demo_fit$model)$most_probable_cluster == rep(c("Class 2", "Class 1"), times = c(500, 200)))

