if (!dir.exists("figures")) {
  dir.create("figures")
}

model_accuracy <- readr::read_tsv(file.path("models", "model-accuracy.tsv"))
model_accuracy$classifier <- factor(
  model_accuracy$classifier,
  c("meta", "multinom", "nb", "nnet", "dnn", "rf", "xgbTree", "C5.0"),
  c("Meta (Bayesian Network)", "Logistic Regression", "Naive Bayes", "Shallow Neural Network", "Deep Neural Network", "Random Forest", "XGB trees", "C5.0 trees")
)

library(ggplot2)

combinations <- expand.grid(
  k = c(2, 3, 5),
  reliability = c("reliable", "unreliable"),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(combinations)) {
  k <- combinations$k[i]
  reliability <- combinations$reliability[i]
  p <- ggplot(
    dplyr::filter(model_accuracy, classes == 2, discernatron_reliable == (reliability == "rel")),
    aes(x = classifier, y = accuracy, color = classifier)
  ) +
    geom_pointrange(aes(ymin = 0, ymax = accuracy)) +
    geom_text(aes(label = sprintf("%.1f%%", 100 * accuracy)), vjust = "bottom", nudge_y = 0.05) +
    facet_grid(features ~ question) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = NULL, y = "Accuracy", color = "Classifier",
      title = "Classification accuracy by question and features used",
      subtitle = glue::glue("Classification accuracy of models trained on {k} classes based on {reliability} Discernatron data"),
      caption = "Meta is a super-learner trained on the predictions of the base learners"
    ) +
    wmf::theme_facet(14, "Open Sans") +
    theme(strip.text.y = element_text(angle = 0), axis.text.x = element_blank())
  ggsave(
    file.path("figures", glue::glue("accuracy_{reliability}-{k}.png")), p,
    dpi = 600, height = 15, width = 30
  )
}; rm(k, reliability, p, i)

model_accuracy %>%
  dplyr::filter(classes == 2) %>%
  dplyr::mutate(reliability = dplyr::if_else(discernatron_reliable, "Reliable", "Unreliable")) %>%
  dplyr::group_by(question, features, reliability) %>%
  dplyr::summarize(avg = mean(accuracy)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reliability, color = reliability)) +
  geom_linerange(aes(ymin = 0, ymax = avg)) +
  geom_label(aes(label = sprintf("%.2f%%", 100 * avg), y = avg)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(question ~ features) +
  labs(
    color = "Reliability of Discernatron Scores", y = "Average accuracy", x = NULL,
    title = "Binary classifier performance across questions, feature sets, and Discernatron score reliability"
  ) +
  wmf::theme_facet(14, "Source Sans Pro", clean_xaxis = TRUE)

