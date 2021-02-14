summarize_survey() <- function(df) {
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
    ylab("") +
    xlab("Proportion (%)") +
    labs(title = str_wrap(question, 80)) +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, 40))
}