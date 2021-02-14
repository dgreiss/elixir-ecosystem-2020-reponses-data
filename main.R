source("./src/packages.R")
source("./src/utils.R")
source("./src/load_data.R")

long <-
  raw %>%
  mutate(`What tool do you use to format code?` = NA, .after = 162) %>%
  mutate(`What CI/CD tool do you use?` = NA, .after = 150) %>%
  mutate(`How do you deploy Elixir in production?` = NA, .after = 143) %>%
  mutate(`What platform do you use to deploy your code in production?` = NA, .after = 137) %>%
  mutate(`What database do you most often use with Elixir?` = NA, .after = 130) %>%
  mutate(`What tool do you most often use to debug Elixir code?` = NA, .after = 123) %>%
  mutate(`What were challenges to adopting Elixir for your team?` = NA, .after = 112) %>%
  mutate(`What benefits did your team experience by using Elixir?` = NA, .after = 106) %>%
  mutate(`In what capacities is your company using Elixir?` = NA, .after = 92) %>%
  mutate(`Do you subscribe to any Elixir newsletters?` = NA, .after = 82) %>%
  mutate(`Do you listen to any Elixir podcasts?` = NA, .after = 75) %>%
  mutate(`Do you participate in any Elixir forums or communities?` = NA, .after = 66) %>%
  mutate(`How do you prefer to learn a new language?` = NA, .after = 24) %>%
  mutate(`How were you first introduced to Elixir?` = NA, .after = 18) %>%
  mutate(`What language were you using prior to Elixir?` = NA, .after = 8) %>%
  pivot_longer(
    !c(`#`, contains("UTC"), `Network ID`),
    names_to = "question_raw",
    values_to = "answer"
  ) %>%
  janitor::clean_names() %>%
  ##########################
  # Key transformation occurs here
  # Joins the choices and free text answers to the appropriate quesiton
  ##########################
  left_join(questions, by = c("question_raw" = "question")) %>%
  fill(id) %>%
  left_join(questions, by = c("id" = "id")) %>%
  # Remove missing answers
  filter(
    !is.na(answer),
    answer != "Choose not to answer"
  ) %>%
  mutate(
    answer = ifelse(answer == "1", "Yes", answer),
    answer = ifelse(answer == "0", "No", answer)
  ) %>%
  select(number:network_id, question, answer)

survey_plots <-
  long %>%
  group_by(question) %>%
  nest() %>%
  mutate(
    summary = map(data, summarize_survey),
    n = map_int(summary, ~ sum(.x$n)),
    question = paste0(question, " (N = ", n, ")"),
    plot = map2(summary, question, barplot_survey)
  )

questions %>% write_csv("./output/questions.csv", na = "")
long %>% write_csv("./output/survey.csv")