df <- read.csv("data/pair_scores.csv")
fit <- lm(relations_score ~ nominate_distance + same_state, data = df)
