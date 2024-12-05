library(here)
library(data.table)
library(readxl)
library(ggplot2)

theme_memorylab_url <- "https://raw.githubusercontent.com/SlimStampen/theme_memorylab/master/theme_memorylab.R"
source(theme_memorylab_url)


# Pretest
pretest <- read_excel(here("data", "test", "Noorderpoort Pretest Scores.xlsx")) |>
  setDT()

# Remove non-data rows
pretest <- pretest[4:.N]

pretest_long <- melt(pretest, id.vars = "email", variable.name = "component", value.name = "score")
pretest_long[, test := "Pretest"]
pretest_long[, component := as.character(component)]

pretest_long[component == "Aftreken & Optellen", component := "Aftrekken & Optellen"]


# Posttest
posttest <- read_excel(here("data", "test", "Noorderpoort Posttest Scores.xlsx")) |>
  setDT()

# Remove non-data rows
posttest <- posttest[4:.N]

posttest_long <- melt(posttest, id.vars = "email", variable.name = "component", value.name = "score")
posttest_long[, test := "Posttest"]
posttest_long[, component := as.character(component)]

posttest_long[component == "Vermenigvuldigingen", component := "Vermenigvuldigen"]

# Combine
tests <- rbind(pretest_long, posttest_long)
tests[, test := factor(test, levels = c("Pretest", "Posttest"))]
tests[, component := factor(component, levels = c("Delen",
                                                  "Percentage",
                                                  "Cijfers",
                                                  "Breuken",
                                                  "Tafels",
                                                  "Decimalen",
                                                  "Aftrekken & Optellen",
                                                  "Vermenigvuldigen",
                                                  "Afronden",
                                                  "Eenheden",
                                                  "Totaal punten",
                                                  "Cijfer"))]


# Average scores per component
tests_avg <- tests[, .(mean_score = mean(score)), by = .(test, component)]
tests_avg

# Plot individual scores
pos_jitter <- position_jitter(width = .05, height = .05, seed = 0)

ggplot(tests, aes(x = test, y = score)) +
  facet_wrap(~ component, scales = "free_y") +
  # geom_point(alpha = .2, position = pos_jitter) +
  geom_line(aes(group = email), alpha = .2, position = pos_jitter) +
  geom_point(data = tests_avg, aes(y = mean_score), color = colours_memorylab[4], size = 2) +
  geom_line(data = tests_avg, aes(y = mean_score, group = component), color = colours_memorylab[4], linewidth = 1) +
  labs(x = NULL, y = "Score", title = "Score per component (Noorderpoort)") +
  theme_ml()

ggsave(here("output", "test_scores_noorderpoort.png"), width = 8, height = 6)
