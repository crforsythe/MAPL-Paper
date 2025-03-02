library(tidyverse)
library(cowplot)
options(dplyr.width = Inf)

# Custom functions ----

read_and_format <- function(path) {
    sim_name <- basename(path) %>% str_remove('.csv')
    df <- read_csv(path) %>% 
        pivot_longer(names_to = 'trial', values_to = 'll', cols = everything()) %>% 
        separate(trial, into = c('subset', 'model', 'trial'), sep = '_') %>% 
        filter(!is.na(ll)) %>% 
        separate(trial, into = c('drop', 'trial'), sep = '-') %>% 
        select(-drop) %>% 
        mutate(
            simulation = str_replace(sim_name, 'simul-', ''), 
            simulation = str_replace(simulation, '-', '_'), 
            model = str_replace_all(model, '-', '_')
        ) %>% 
        group_by(subset, model, trial, simulation) %>% 
        summarise(ll = min(ll)) %>% 
        ungroup()
    return(df)
}

# Compare models ----

indep_norm <- read_and_format(
    file.path("data", "compare-models", "simul-indep-norm.csv")
)
indep_lognorm <- read_and_format(
    file.path("data", "compare-models", "simul-indep-lognorm.csv")
)
correl_norm <- read_and_format(
    file.path("data", "compare-models", "simul-correl-norm.csv")
)
nonlinear <- read_and_format(
    file.path("data", "compare-models", "simul-indep-norm-nonlinear.csv")
)
interaction <- read_and_format(
    file.path("data", "compare-models", "simul-indep-norm-inter.csv")
)

df <- bind_rows(indep_norm, indep_lognorm, correl_norm, nonlinear, interaction) %>% 
    mutate(
        simulation = str_replace_all(simulation, '-', '_'), 
        model = ifelse(model == 'mxl_misspec', 'mxl_ind_norm', model)
    ) %>% 
    # Don't include training runs
    filter(subset == 'eval')

df_diff <- df %>%
    pivot_wider(names_from = model, values_from = ll) %>% 
    pivot_longer(names_to = 'model', values_to = 'll', -c('subset', 'trial', 'simulation', 'mxl')) %>%
    filter(!is.na(ll)) %>% 
    mutate(
        diff = ll - mxl,
        diff_p = diff / mxl
    ) %>% 
    mutate(
    model = fct_recode(model, 
        'MNL' = 'mnl', 
        'MXL Ind. Normal' = 'mxl_ind_norm', 
        'Simple NN' = 'classic_nn', 
        'Deep NN' = 'classic_dnn', 
        'MAPL Normal' = 'havan_normal',
        'MAPL FM' = 'havan_fm'
    ),
    model = fct_relevel(model, rev(c(
        'MNL', 'MXL Ind. Normal', 'Simple NN', 'Deep NN',
        'MAPL Normal', 'MAPL FM'
    ))),
    simulation = fct_recode(simulation, 
        'Independent Normals' = 'indep_norm', 
        'Independent Log-normals' = 'indep_lognorm', 
        'Correlated Normals' = 'correl_norm', 
        'Independent Normals w/Interaction' = 'indep_norm_inter', 
        'Independent Normals w/Nonlinearity' = 'indep_norm_nonlinear'
    ),
    simulation = fct_relevel(simulation, c(
        'Independent Normals', 'Independent Log-normals', 
        'Correlated Normals', 'Independent Normals w/Interaction', 
        'Independent Normals w/Nonlinearity'
    ))
  )

# Quick summary
df_diff %>% 
    group_by(simulation, model) %>%
    summarise(mean(diff_p))
    # filter(model == 'MAPL Normal')

plot <- df_diff %>% 
    filter(subset == 'eval') %>%
    filter(simulation != 'Independent Log-normals') %>% 
    # filter(model != 'Deep NN') %>% 
    mutate(
        simulation = fct_relevel(simulation, rev(levels(simulation))),
        model = as.character(model),
        model = ifelse(model == 'MAPL Normal', 'MAPL (Normal)', model),
        model = ifelse(model == 'MAPL FM', 'MAPL (Fosgerau-Mabit)', model),
        model = fct_relevel(model, c(
            "MNL", "MXL Ind. Normal",
            "Simple NN", "Deep NN",
            "MAPL (Normal)", "MAPL (Fosgerau-Mabit)"
        ))
        # model = paste0('Model: ', model), 
        # model = fct_relevel(model, c(
        #     "Model: MNL", "Model: MXL Ind. Normal", 
        #     "Model: Simple NN", "Model: Deep NN", 
        #     "Model: MAPL (Normal)", "Model: MAPL (Fosgerau-Mabit)"
        # ))
    ) %>% 
    ggplot() +
    geom_boxplot(
        aes(x = diff_p, y = simulation), 
        fill = 'white', width = 0.7
    ) + 
    scale_x_continuous(
        labels = scales::percent, 
        limits = c(-0.007, 0.23)
    ) +
    geom_vline(xintercept = 0, color = 'black') +
    facet_wrap(vars(model), ncol = 1) + 
    theme_minimal_vgrid(font_family = 'Roboto Condensed') + 
    theme(
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = NA), 
        strip.background = element_rect(fill = "gray80"), 
        plot.title.position = 'plot'
    ) +
    labs(
        title = 'Percent difference between estimated log-likelihood of select models\nand that of the true data-generating mixed logit model',
        subtitle = 'The MAPL model achieves superior performance without needing to assume a utility model form or\nfeature-level heterogeneity distributional forms. The Fosgerau-Mabit (FM) aggregate heterogeneity distribution\nout-performs the normal distribution given its greater flexibility.',
        x = 'Percentage difference in estimated log-likelihood\nrelative to that of the true data-generating mixed logit model',
        y = 'Data generating scenario'
    )

plot

ggsave(
    file.path('figs', 'models_boxplot_percent.png'), 
    width = 7.5, 
    height = 8
)

# Version with no subtitle

plot +
    labs(subtitle = NULL)

ggsave(
    file.path('figs', 'paper_models_boxplot_percent.png'), 
    width = 7.5, 
    height = 8
)

# Condensed version

df_diff %>% 
    filter(subset == 'eval') %>%
    filter(simulation != 'Independent Log-normals') %>% 
    filter(model != 'Deep NN') %>% 
    mutate(
        simulation = fct_relevel(simulation, rev(levels(simulation)))
    ) %>% 
    ggplot() +
    geom_boxplot(
        aes(x = diff_p, y = model), 
        width = 0.6
    ) + 
    geom_jitter(
        aes(x = diff_p, y = model), 
        size = 0.2, height = 0.2
    ) + 
    scale_x_continuous(
        labels = scales::percent, 
        limits = c(-0.007, 0.155)
    ) +
    theme_minimal_vgrid(font_family = 'Roboto Condensed') + 
    theme(
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = NA), 
        strip.background = element_rect(fill = "gray80"), 
        plot.title.position = 'plot'
    ) +
    labs(
        title = 'Percent difference between estimated log-likelihood of select models and\nthat of the true data-generating mixed logit model',
        subtitle = 'Results pooled across all four data-generating processes',
        x = 'Percent difference in estimated log-likelihood\nrelative to that of the true data-generating mixed logit model',
        y = 'Model'
    )

ggsave(
    file.path('figs', 'models_boxplot_percent_total.png'), 
    width = 7, 
    height = 5
)

# Compare sample sizes ----

indep_norm_10000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_10000-indiv.csv")
)
indep_norm_9000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_9000-indiv.csv")
)
indep_norm_8000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_8000-indiv.csv")
)
indep_norm_7000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_7000-indiv.csv")
)
indep_norm_6000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_6000-indiv.csv")
)
indep_norm_5000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_5000-indiv.csv")
)
indep_norm_4000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_4000-indiv.csv")
)
indep_norm_3000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_3000-indiv.csv")
)
indep_norm_2000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_2000-indiv.csv")
)
indep_norm_1000 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_1000-indiv.csv")
)
indep_norm_500 <- read_and_format(
    file.path("data", "compare-size", "simul-indep-norm_500-indiv.csv")
)

df <- bind_rows(
    indep_norm_10000, indep_norm_9000, indep_norm_8000, indep_norm_7000,
    indep_norm_6000, indep_norm_5000, indep_norm_4000, indep_norm_3000,
    indep_norm_2000, indep_norm_1000, indep_norm_500
) %>% 
    mutate(simulation = str_replace_all(simulation, '-', '_')) %>% 
    # Don't include training runs
    filter(subset == 'eval')

df_diff <- df %>% 
    mutate(simulation = parse_number(simulation)) %>% 
    pivot_wider(names_from = model, values_from = ll) %>% 
    pivot_longer(
        names_to = 'model', 
        values_to = 'll', 
        cols = -c('subset', 'trial', 'simulation', 'mxl')
    ) %>%
    filter(!is.na(ll)) %>% 
    mutate(
        diff = ll - mxl,
        diff_p = diff / mxl
    )

plot <- df_diff %>% 
    filter(subset == 'eval') %>%
    mutate(
        sim_n = simulation,
        simulation = as.factor(scales::comma(simulation)), 
        simulation = fct_reorder(simulation, sim_n)
    ) %>% 
    ggplot() +
    geom_boxplot(
        aes(x = diff_p, y = simulation), 
        fill = 'steelblue', width = 0.7
    ) + 
    scale_x_continuous(
        labels = scales::percent, 
        limits = c(0, 0.06)
    ) +
    geom_vline(xintercept = 0, color = 'black') +
    theme_minimal_vgrid(font_family = 'Roboto Condensed') + 
    theme(
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = NA), 
        strip.background = element_rect(fill = "gray80"), 
        plot.title.position = 'plot'
    ) +
    labs(
        title = 'Percent difference between estimated log-likelihood of MAPL-FM model and\n that of the true data-generating mixed logit model for different sample sizes',
        subtitle = 'At lower sample sizes, the MAPL-FM model has greater error relative to the true model.',
        x = 'Percentage difference in estimated log-likelihood\nrelative to that of the true data-generating mixed logit model',
        y = 'Sample size'
    )

plot

ggsave(
    file.path('figs', 'size_boxplot_percent.png'), 
    width = 7.5, 
    height = 8
)

plot <- df_diff %>% 
    filter(subset == 'eval') %>%
    mutate(
        sim_n = simulation,
        simulation = as.factor(scales::comma(simulation)), 
        simulation = fct_reorder(simulation, sim_n)
    ) %>% 
    ggplot() +
    geom_boxplot(
        aes(x = diff_p, y = simulation), 
        width = 0.6,
        outlier.shape = NA
    ) + 
    geom_jitter(
        aes(x = diff_p, y = simulation), 
        size = 0.2, height = 0.2
    ) + 
    scale_x_continuous(
        labels = scales::percent, 
        limits = c(0, 0.06)
    ) +
    theme_minimal_vgrid(font_family = 'Roboto Condensed') + 
    theme(
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = NA), 
        strip.background = element_rect(fill = "gray80"), 
        plot.title.position = 'plot'
    ) +
    labs(
        title = 'Percent difference between estimated log-likelihood of MAPL model and\nthat of the true data-generating mixed logit model for different sample sizes',
        subtitle = 'At lower sample sizes, the MAPL model has greater error relative to the true model.',
        x = 'Percent difference in estimated log-likelihood\nrelative to that of the true data-generating mixed logit model',
        y = 'Sample size'
    )

plot

ggsave(
    file.path('figs', 'size_boxplot_percent_total.png'), 
    width = 7.5, 
    height = 8
)

# Version with no subtitle

plot +
    labs(subtitle = NULL)

ggsave(
    file.path('figs', 'paper_size_boxplot_percent_total.png'), 
    width = 7.5, 
    height = 8
)
