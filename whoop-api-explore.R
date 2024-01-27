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

auth_code <- oauth_flow_auth_code( client = oauth_client,
                                     auth_url = auth_code_url,
                           redirect_uri = "http://localhost:9090",
                           scope ="read:cycles")

req_test <- request("https://api.prod.whoop.com/developer/v1/cycle") |>
    req_auth_bearer_token(auth_code$access_token) |>
    req_perform_iterative(iterate_with_offset("next_token"),
                          max_reqs = 10)

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
  ggplot(aes(created_at, average_heart_rate)) + geom_line() + theme_minimal()
