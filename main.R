library(tidyverse)
library(ggplot2)
library(gridExtra)

raw <- read_csv("elixir-ecosystem-survey-2020-raw-data.csv", col_types = cols(.default = "c"))

additions <- c(
  "What tool do you use to format code?",
  "What CI/CD tool do you use?",
  "How do you deploy Elixir in production?",
  "What platform do you use to deploy your code in production?",
  "What database do you most often use with Elixir?",
  "What tool do you most often use to debug Elixir code?",
  "What were challenges to adopting Elixir for your team?",
  "What benefits did your team experience by using Elixir?",
  "In what capacities is your company using Elixir?",
  "Do you subscribe to any Elixir newsletters?",
  "Do you listen to any Elixir podcasts?",
  "Do you participate in any Elixir forums or communities?",
  "How do you prefer to learn a new language?",
  "How were you first introduced to Elixir?"
)

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
  pivot_longer(
    !c(`#`, contains("UTC"), `Network ID`),
    names_to = "question",
    values_to = "answer"
  ) %>%
  janitor::clean_names()

questions_with_choices <-
  long %>%
  distinct(question)

qs <- c(
  "Are you actively using Elixir in either professional or personal projects?",
  "What solutions was the Elixir ecosystem lacking?",
  "How long have you been using Elixir?",
  "What is the most recent version of Elixir that you have used?",
  "Have you written any Erlang?",
  "What is your age range?",
  "Which gender do you identify as?",
  "In which country do you currently reside?",
  "Do you have a college degree in Computer Science or similar degree?",
  "What part of Elixir did you find most difficult to learn?",
  "Do you maintain any Open Source (OSS) Elixir libraries?",
  "Have you made contributions to anyone else's OSS Elixir libraries?",
  "Have you made OSS contributions back to Elixir?",
  "How often do you attend local Elixir meetups?",
  "Do you help organize Elixir meetups?",
  "Do you attend your continent's major Elixir Conference",
  "Do you attend any regional Elixir/Erlang conferences?",
  "What industry is your company in?",
  "What is your role within your company?",
  "Does your company use Elixir?",
  "How long has your company been using Elixir?",
  "How many engineers are using Elixir at your company?",
  "Did your company migrate from another language or choose Elixir for a new project?",
  "Can you say which language(s) and describe why it won?",
  "Which operating system do you primarily develop on?",
  "Which editor/IDE do you primarily write Elixir with?",
  "Which operating system do you deploy to?",
  "Have you ever used Hot Code Reloading in production?",
  "If there is one library that you are excited about in 2020 which is it?",
  "Are you using Phoenix?",
  "What is the most recent version of Phoenix that you have used?",
  "Are you running Phoenix in production?",
  "Are you using Nerves?",
  "What is the most recent version of Nerves that you have used?",
  "Are you using Scenic?",
  "Is your Nerves application distributed across many devices?",
  additions
)

questions <- tibble(id = seq_along(qs), question = qs)

choices <-
  questions_with_choices %>%
  left_join(questions, by = c("question" = "question")) %>%
  fill(id) %>%
  anti_join(questions, by = c("question" = "question")) %>%
  rename(question_id = id) %>%
  rename(choice = question) %>%
  mutate(id = seq_len(nrow(.))) %>%
  select(id, question_id, choice)

summarize_survey <- function(df) {
  df %>%
    mutate(answer = forcats::fct_lump_n(answer, 14, ties.method = "first")) %>%
    group_by(answer) %>%
    summarize(n = n()) %>%
    mutate(prop = n / sum(n)) %>%
    arrange(desc(prop))
}

barplot_survey <- function(df, question) {
  ggplot(df, aes(prop, reorder(answer, -prop))) +
    geom_col(width = 0.4, fill = "steelblue") +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), hjust = -0.10) +
    theme_classic() +
    #     ggtitle(question) +
    ylab("") +
    xlab("Proportion (%)") +
    labs(title = str_wrap(question, 80)) +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, 40))
}

survey_plots <-
  long %>%
  inner_join(questions, by = c("question" = "question")) %>%
  filter(
    !(question %in% additions),
    !is.na(answer),
    answer != "Choose not to answer"
  ) %>%
  replace_na(list(answer = "Did not answer.")) %>%
  mutate(
    answer = ifelse(answer == "1", "Yes", answer),
    answer = ifelse(answer == "0", "No", answer)
  ) %>%
  group_by(question) %>%
  nest() %>%
  mutate(
    summary = map(data, summarize_survey),
    n = map_int(summary, ~ sum(.x$n)),
    question = paste0(question, " (N = ", n, ")"),
    plot = map2(summary, question, barplot_survey)
  )