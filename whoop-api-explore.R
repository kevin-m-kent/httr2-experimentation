library(httr2)
library(AzureKeyVault)
library(purrr)
library(tidyverse)
library(lubridate)

vault <- key_vault("https://whoop.vault.azure.net/",
                   tenant = Sys.getenv("AZURE_TENANT"))

client_id <- vault$secrets$get('whoop-client')$value
client_secret <- vault$secrets$get('whoop-secret')$value

auth_code_url <- "https://api.prod.whoop.com/oauth/oauth2/auth"
token_url <- "https://api.prod.whoop.com/oauth/oauth2/token"

oauth_client <- oauth_client(client_id, token_url, secret=client_secret)

api_base <- "https://api.prod.whoop.com/developer/"

req_test <- request(paste0(api_base, "v1/activity/sleep")) |>
    req_oauth_auth_code(oauth_client,
                        auth_url = auth_code_url,
                        redirect_uri = "http://localhost:9090",
                        scope ="read:sleep") |>
    req_perform_iterative(iterate_with_cursor("nextToken", function(resp)
                                                  {
                                                 resp |>
                                                    resp_body_json() |>
                                                    pluck("next_token")}))

get_records <- function(resp) {

  resp |>
    resp_body_json() |>
    pluck("records")

}

data_parsed <- req_test |>
  resps_data(get_records) |>
  map_dfr(~ enframe(.) |> pivot_wider()) |>
  unnest_wider(col="score") |>
  mutate(across(contains("_at"), ymd_hms)) |>
  mutate(across(c("start", "end"), ymd_hms)) |>
  mutate(across(contains("id"), as.numeric))


data_parsed |>
  ggplot(aes(created_at, sleep_performance_percentage)) + geom_line() + theme_minimal()  +
  labs(title = "Sleep Performance",
       subtitle = "Source: Whoop API",
       x = "Created", y = "Sleep Peformance Percentage")

data_parsed |>
 mutate(wkday = wday(created_at)) |>
  mutate(created_at = floor_date(created_at, "week")) |>
 ggplot(aes(created_at, wkday, fill = sleep_performance_percentage)) + geom_tile() +
  labs(title = "Sleep Performance",
       subtitle = "Source: Whoop API",
       x = "Created", y = "Sleep Peformance Percentage")

ggsave(here::here("Plots", "average_sleep_perf.png"))
