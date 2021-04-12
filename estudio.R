require("ggplot2")
require("dplyr")

df_commits <- read.csv("/home/alf/Descargas/commits.csv")

# The graph
names(df_commits) <- c('university', 'task_type', 'commit_qty')

df_commits %>% 
  group_by(task_type, university) %>%
  summarise(mean_t = mean(commit_qty, na.rm = T),
    n=n(),
    se_t = sd(commit_qty, na.rm = T)/sqrt(n),
    commit_max = max(commit_qty),
    commit_min = min(commit_qty)
  ) %>%
  ungroup() %>%
  mutate(task_type = factor(task_type, levels=c('Tradicional (2018)', 'Juego (2019)'))) %>%
  ggplot(aes(x=task_type, y=mean_t)) + 
    geom_errorbar(aes(ymin=commit_min, ymax=commit_max), width=0.2) +
    geom_crossbar(aes(ymin= mean_t - se_t, ymax= mean_t + se_t, color=task_type)) +
    facet_wrap(~university) +
    theme(legend.position = "none") + # oculta las referencias
    xlab("Tipo de tarea") +
    ylab("Cantidad de Commits") +
    ggtitle("Cantidad de commits por tipo de tarea")