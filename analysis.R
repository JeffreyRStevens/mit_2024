library(tidyverse)
library(ggreveal)



# Brady et al. 2018 ----
# Adult 1
brady_adult1_max_dist <- c(0.25, 1.5, 1.75, 1.75, 2, 2, 2.25, 2.5, 2.75, 3, 3, 3.25, 3.5, 3.5, 3.75, 4, 4.5, 4.5, 3.5, 5.75, 6.75, 7.5, 3.25, 1)
brady_adult1_dias_overall <- c(0.74, 0.53, 0.39, 0.67, 0.74, 0.42, 0.34, 0.52, 0.49, 0.6, 0.65, 0.63, 0.5, 0.49, 0.59, 0.47, 0.53, 0.49, 0.64, 0.41, 0.46, 0.45, 0.61, 0.54)
brady_full_corr <- cor.test(brady_adult1_max_dist, brady_adult1_dias_overall)

brady_adult1_max_dist_trimmed <- brady_adult1_max_dist[-7]
brady_adult1_dias_overall_trimmed <- brady_adult1_dias_overall[-7]


# Adult 2
brady_adult2_max_dist <- c(1, 0.75, 5, 1.5, 1.5, 0.75, 0.75, 1, 0.25, 3.75, 3.25, 0.5, 4.5)
brady_adult2_dias_overall <- c(0.56, 0.62, 0.43, 0.59, 0.49, 0.64, 0.49, 0.62, 0.49, 0.5, 0.5, 0.58, 0.43)
brady_adult2_ages <- c(108, 108, 84, 48, 36, 48, 72, 84, 60, 72, 96, 36, 48)


# Pup
brady_pup_max_dist <- c(2, 1.25, 0.25, 2.5, 5.25, 2.75, 7, 1.7, 3, 0, 2, 2.75, 1.5, 3.25, 5.5, 3.25, 4.75, 0, 0.25, 1.25, 0, 3, 5)
brady_pup_dias_overall <- c(0.59, 0.56, 0.59, 0.56, 0.52, 0.47, 0.61, 0.66, 0.44, 0.58, 0.46, 0.69, 0.46, 0.54, 0.53, 0.59, 0.57, 0.53, 0.59, 0.46, 0.55, 0.47, 0.52)

brady_data <- data.frame(study = c(rep("Adult study 1", length(brady_adult1_max_dist)),
                                   rep("Adult study 2", length(brady_adult2_max_dist)),
                                   rep("Pup study", length(brady_pup_max_dist))),
                         max_dist = c(brady_adult1_max_dist, brady_adult2_max_dist, brady_pup_max_dist),
                         dias_overall = c(brady_adult1_dias_overall, brady_adult2_dias_overall, brady_pup_dias_overall))

brady_plot <- brady_data |> 
  ggplot(aes(x = dias_overall, y = max_dist)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", color = "#0072B2") +
  facet_wrap(vars(study)) +
  labs(x = "Owner perceptions of impulsivity (DIAS)",
       y = "Maximum distance travelled (m)") +
  theme_bw()
brady_plotlist <- reveal_panels(brady_plot)

reveal_save(brady_plotlist, here::here("media/bradyetal2018.png"), width = 8, height = 4, scale = 0.8)




  # ManyDogs 1 --------------------------------------------------------------

md_data <- read_csv("manydogs_etal_2024_data.csv")

expdata <- md_data |> 
  filter(experiment_status == "Included") |> 
  select(site, subject_id, contains("ostensive")) |> 
  rowwise() |> 
  mutate(ostensive = mean(c_across(starts_with("ostensive")), na.rm = TRUE),
         nonostensive = mean(c_across(starts_with("nonostensive")), na.rm = TRUE),
         ost_nonost = ostensive - nonostensive) |> 
  select(site, subject_id, ostensive, nonostensive, ost_nonost)

site_plot <- expdata |> 
  ggplot(aes(x = fct_reorder(site, ost_nonost, .fun = mean), y = ost_nonost)) +
  # geom_point(color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun.data = mean_cl_normal) +
  coord_flip() +
  labs(x = "Research site", y = "Bias toward communicative accuracy") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))
site_plotlist <- reveal_y(site_plot)
reveal_save(site_plotlist, "media/md1_site_accuracy.png", width = 6, height = 4, scale = 0.8)

expdata_long <- expdata |> 
  select(-ost_nonost) |> 
  pivot_longer(cols = contains("ostensive"), names_to = "condition", values_to = "accuracy")

acc_plot <- expdata_long |> 
  ggplot(aes(x = condition, y = accuracy)) +
  geom_line(aes(group = subject_id), color = "grey90") +
  geom_point(color = "grey") +
  stat_summary(fun.data = mean_cl_normal) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_discrete(labels = c("Non-communicative", "Communicative")) +
  labs(x = "Condition", y = "Accuracy") +
  theme_bw() +
  theme(text = element_text(family = "Arial"))
acc_plotlist <- reveal_layers(acc_plot)
reveal_save(acc_plotlist, "media/md1_overall_accuracy.png", width = 6, height = 4, scale = 0.8)
