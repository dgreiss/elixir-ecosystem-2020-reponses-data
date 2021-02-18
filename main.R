source("./src/packages.R")
source("./src/utils.R")
source("./src/load_data.R")

long <-
  raw %>%
  mutate(`How are you using Phoenix?` = NA, .after = 170) %>%
  mutate(`If your team is enforcing code formatting what methods do you use?` = NA, .after = 162) %>%
  mutate(`If your project(s) uses Continuous Integration which is it?` = NA, .after = 150) %>%
  mutate(`How are you deploying your Elixir application?` = NA, .after = 143) %>%
  mutate(`Where do you deploy your Elixir applications to?` = NA, .after = 137) %>%
  mutate(`What database do you primarily use?` = NA, .after = 129) %>%
  mutate(`How do you debug?` = NA, .after = 123) %>%
  mutate(`Which reasons prevented Elixir from being adopted at your company?` = NA, .after = 112) %>%
  mutate(`What outcomes did your team experience after adopting Elixir?` = NA, .after = 106) %>%
  mutate(`Did your company migrate from another language / framework?` = NA, .after = 97) %>%
  mutate(`In what capacities is your company using Elixir?` = NA, .after = 92) %>%
  mutate(`Which Elixir newsletters are you subscribed to?` = NA, .after = 82) %>%
  mutate(`Which Elixir Podcasts do you listen to?` = NA, .after = 75) %>%
  mutate(`Do you participate in any online Elixir chats?` = NA, .after = 66) %>%
  mutate(`Have you written in any other BEAM based language?` = NA, .after = 36) %>%
  mutate(`What sources of information was most impactful for learning Elixir?` = NA, .after = 24) %>%
  mutate(`How did you first learn about Elixir?` = NA, .after = 18) %>%
  mutate(`Which language did you change to?` = NA, .after = 8) %>%
  mutate(`Can you share why you stopped using or never used Elixir?` = NA, .after = 2) %>%
  pivot_longer(
    !c(`#`, contains("UTC", ignore.case = FALSE), `Network ID`),
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
  rename(question_id = id) %>%
  rename(user_id = number) %>%
  select(user_id:network_id, question_id, question, answer)

survey_plots <-
  long %>%
  arrange(question_id) %>%
  group_by(question_id, question) %>%
  nest() %>%
  mutate(
    summary = map(data, summarize_survey),
    n = map_int(summary, ~ sum(.x$n)),
    question = paste0(question, " (N = ", n, ")"),
    plot = map2(summary, question, barplot_survey)
  )

questions %>% write_csv("./output/questions.csv", na = "")
long %>% write_csv("./output/survey.csv")

survey_plots %>%
  ungroup() %>%
  select(question_id, summary) %>%
  unnest(c(summary)) %>%
  write_csv("./output/survey_summary.csv", na = "")