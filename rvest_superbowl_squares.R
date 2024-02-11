library(rvest)
library(tidyverse)

# from https://www.footballdb.com/seasons/super-bowls.html
raw <- read_html(here::here("Data", "superbowlhistory.html"))

cleaned_table <- raw |>
  html_elements("table") |>
  html_table() |>
  bind_rows(.id="id")

names(cleaned_table) <- c("id", "team", "q1", "q2", "q3", "q4", "final", "ot")

possible_vals <- crossing(away = 0:9, home = 0:9, name = c("q1", "q2", "q3", "final"))

order_df <- tibble(
  name = c("q1", "q2", "q3", "final"),
  order = 1:4
)

cleaned_table |>
  group_by(id) |>
  mutate(team_no = c("away", "home")) |>
  pivot_longer(c(-team, -id, -team_no)) |>
  filter(!(name %in% c("ot", "q4"))) |>
  mutate(value = value %% 10) |>
  select(id, name, team_no, value) |>
  pivot_wider(names_from = "team_no", values_from = "value") |>
  ungroup() |>
  count(name, away, home) |>
  full_join(possible_vals) |>
  replace_na(list(n = 0)) |>
  drop_na() |>
  left_join(order_df) |>
  mutate(name = fct_reorder(name, order)) |>
  mutate(home = factor(home, levels = c(1, 8, 4, 7, 3, 2, 0, 6, 5, 9)),
         away = factor(away, levels = c(3, 6, 7, 2, 1, 5, 4, 9, 8, 0))) |>
  ggplot(aes(away, home, fill = n)) + geom_tile()  + facet_wrap(~name, scales = "free", strip.position = "top") +
  theme(axis.text.x.top = element_text(angle = 0, hjust = 1), # Adjusts x-axis labels on top to be horizontal
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines"),
        axis.title.x.top = element_text(margin = margin(5, 0, 5, 0))) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(vjust = 0.5)) +
  labs(x = NULL, y = NULL, title = "Super Bowl Squares Frequency - Historical",
       subtitle = paste0("Data from https://www.footballdb.com/seasons/super-bowls.html")) +
  scale_x_discrete(position = "top")


ggsave(here::here("Plots", "superbowl_squares_historical.png"), width = 12, height = 12)
