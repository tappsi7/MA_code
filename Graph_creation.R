rm(list= ls())

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Nigeria")

library(tidyverse)
library(sf)
library(haven) 
library(patchwork)
library(cowplot)
library(purrr)
library(knitr)
library(kableExtra)
library(INLA)

#################################################################################
#################################################################################
## This code creates all the graphs and tables 

#Creating the maps to show clusters and covariate distribution pre region
# The code  might be weird but combining the maps into one graph was hard
shp_files <- list.files(
  path = "../", 
  pattern = "\\.shp$", 
  recursive = TRUE, 
  full.names = TRUE
)


all_shp <- map_dfr(shp_files, function(fp) {
  country <- basename(dirname(dirname(fp)))  # 
  
  st_read(fp, quiet = TRUE) %>%
    mutate(country = country)
}) %>%
  select(DHSCLUST, DHSYEAR, geometry, LATNUM, LONGNUM, URBAN_RURA, country)
  
all_shp <- all_shp %>% mutate(country = case_when(country == "Sierra_Leone" ~ "Sierra Leone",
                                                  country == "Chad" ~ "Tchad",
                                                  country == "Guinea" ~ "Guinée",
                                                  TRUE ~ country),
                              strata = case_when(URBAN_RURA == "R" ~ "Rural",
                                                 URBAN_RURA == "U" ~ "Urban"))

adm <- readRDS("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/adm.rds")



all_shp <- st_transform(all_shp, crs = st_crs(adm))

plots <- split(adm, adm$Country) %>%
  purrr::imap(function(admin_sub, country_name) {
    
    points_sub <- all_shp %>%
      dplyr::filter(country == country_name)
    
    bbox <- st_bbox(admin_sub)
    
    ggplot() +
      geom_sf(data = admin_sub, fill = "grey90", color = "black", linewidth = 0.2) +
      geom_sf(
        data = points_sub,
        aes(color = strata),
        size = 0.2,
        alpha = 0.4
      ) +
      scale_color_manual(
        values = c(
          "Urban" = "#e34a33",
          "Rural" = "#08519c"
        )
      ) +
      coord_sf(
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      ) +
      theme_void() +
      ggtitle(country_name) +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none" 
      )
  })

legend_plot <- ggplot() +
  geom_sf( 
    data = adm,
    aes(fill = "Administrative regions"),
    color = "black",
    linewidth = 0.2
  ) +
  geom_sf(
    data = all_shp,
    aes(color = strata),
    size = 3,
    alpha = 0.6
  ) +
  scale_fill_manual(
    name = "",
    values = c("Administrative regions" = "grey90")
  ) +
  scale_color_manual(
    values = c(
      "Urban" = "#e34a33",
      "Rural" = "#08519c"
    )) + 
  coord_sf(
    expand = FALSE
  ) +
  theme_void() +
  ggtitle("Plot") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

design <- "A#B#C#D
           E#F#G#H
           I#J#K##"



legend <- get_legend(
  legend_plot + theme(legend.position = "right")
)

legend_plot <- ggdraw(legend)

plots[[length(plots) + 1]] <- legend_plot

plots[c(3, 4)] <- plots[c(4, 3)]
plots[c(7, 8)] <- plots[c(8, 7)]

DHS_plot  <- wrap_plots(plots, col = 6) +
  plot_layout(design = design) +
  plot_annotation(
    title = "Survey Clusters over Administrative Regions:",
    caption = "Source: DHS survey https://dhsprogram.com/",
    theme = theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5  # center
      )
    )
  )

ggsave(
  "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/DHS_output.jpg",
  DHS_plot,
  width = 18,
  height = 13,
  units = "cm",
  dpi = 300
)

covariates <- readRDS("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/covariates.rds")

covar <- covariates %>%
  group_by(adm, Country) %>%
  summarise(
    hum_aid = sum(hum_aid, na.rm = TRUE),
    dev_aid = sum(dev_aid, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    geometry = st_union(geometry.x)
  )

max(log10(covar$hum_aid + 1))
covar <- st_as_sf(covar)

covar$Country <- str_to_title(covar$Country)

plots <- split(covar, covar$Country) %>%
  purrr::imap(function(admin_sub, country_name) {
    
    bbox <- st_bbox(admin_sub)
    
    ggplot() +
      geom_sf(
        data = admin_sub,
        aes(fill = log10(hum_aid + 1)),
        color = "black",
        linewidth = 0.2
      ) +
      scale_fill_gradient(
        name = "Humanitarian aid (log)",
        low = "#a8e6cf",   # mint green
        high = "#08519c",  # deep blue
        limits = c(0, 10),
        oob = scales::squish,
        na.value = "grey"
      ) +
      coord_sf(
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      ) +
      theme_void() +
      ggtitle(country_name) +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none"   
      )
  })

legend_plot <- ggplot() +
  geom_sf(
    data = covar,
    aes(fill = log10(hum_aid + 1)),
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    name = "Humanitarian aid (log 10)",
    low = "#a8e6cf",   # mint green
    high = "#08519c",  # deep blue
    limits = c(0, 10),
    oob = scales::squish,
    na.value = "grey"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "none"   
  )

legend <- get_legend(
  legend_plot + theme(legend.position = "right")
)

legend_plot <- ggdraw(legend)

plots[[length(plots) + 1]] <- legend_plot



design <- "A#B#C#D
           E#F#G#H
           I#J#K##"

Hum_plot  <- wrap_plots(plots, col = 6) +
  plot_layout(design = design) +
  plot_annotation(
    title = "Log total amount of humanitarian aid per region",
    subtitle = "The logged amount of humanitarian aid recieved over all observed years per region",
    caption = "Findley et. al. (2011)",
    theme = theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5  # center
      )
    )
  )

ggsave(
  "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/Hum_output.jpg",
  Hum_plot,
  width = 18,
  height = 13,
  units = "cm",
  dpi = 300
)

## The Death plot

plots <- split(covar, covar$Country) %>%
  purrr::imap(function(admin_sub, country_name) {
    
    bbox <- st_bbox(admin_sub)
    
    ggplot() +
      geom_sf(
        data = admin_sub,
        aes(fill = log10(deaths + 1)),
        color = "black",
        linewidth = 0.2
      ) +
      scale_fill_gradient(
        name = "Battle Related Deaths (log 10)",
        low = "#a8e6cf",   # mint green
        high = "#08519c",  # deep blue
        limits = c(0, 5),
        oob = scales::squish,
        na.value = "grey"
      ) +
      coord_sf(
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      ) +
      theme_void() +
      ggtitle(country_name) +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none"   
      )
  })

legend_plot <- ggplot() +
  geom_sf(
    data = covar,
    aes(fill = log10(deaths +1)),
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    name = "Battle deaths (log 10)",
    low = "#a8e6cf",   # mint green
    high = "#08519c",  # deep blue
    limits = c(0, 5),
    oob = scales::squish,
    na.value = "grey"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "none"   
  )

legend <- get_legend(
  legend_plot + theme(legend.position = "right")
)

legend_plot <- ggdraw(legend)

plots[[length(plots) + 1]] <- legend_plot



design <- "A#B#C#D
           E#F#G#H
           I#J#K##"

Hum_plot  <- wrap_plots(plots, col = 6) +
  plot_layout(design = design) +
  plot_annotation(
    title = "Log total amount of battle related deaths per region",
    subtitle = "The logged total amount of battle related deaths over all years per region",
    caption = "Davis et. all. (2025)",
    theme = theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5  # 
      )
    )
  )

ggsave(
  "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/Death_output.jpg",
  Hum_plot,
  width = 18,
  height = 13,
  units = "cm",
  dpi = 300
)
plots[2]


# The end of the plot that makes the maps
###########################################################################
###########################################################################
# Creating the summary table for the surveys 
summary <- data.frame(
  Country = c("Angola", "Burundi", "Chad", "Ethiopia", "Guinée", "Lesotho",
              "Liberia", "Nigeria", "Sierra Leone", "Uganda"),
  DHS = c("2015-16, 2023-24", "2010, 2016-17", "2014-15", "2000, 2005, 2011, 2016, 2019",
          "1999, 2005, 2012, 2018", "2004, 2009, 2014", "2007, 2013, 2019-20",
          "1990, 2003, 2008, 2013", "2008, 2013, 2019", "2000-01, 2006, 2011, 2016"),
  Aiddata = c("1989 - 1996, 1998 - 2003", "1991 - 2006, 2008", "1989 - 1995, 1997- 2003, 2005 - 2008",
              "1989 - 2008", "2000 - 2002", "1998 - 1999", "1989 - 1997, 2000 - 2004", "2003 - 2005", 
              "1991 - 2001", "1989 - 2008"),
  Cluster_Year = c(51930, 88319, 60312, 193104,16768, 11478, 59619, 33669, 72384, 167153 )
)





kable(
  summary,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Information by Country",
  col.names = c(
    "Country",
    "Included DHS surveys",
    "Coverage of aid data by year",
    "Total Number of Cluster-Year obsrevations"
  )
) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 10
  )
################################################################################
################################################################################
# Creating the model related tables and plots. The prior predictive checks, fit 
# check forest plot and model table

rm(list = ls())

# ---- Setting up ----
count_data <- readRDS("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/all_counts.rds")  # adjust path
model1 <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_1_final.rds")
model2 <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_2_final.rds")
mat <- readRDS("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/big_mat.rds")

mat <- mat[order(rownames(mat)), 
           order(colnames(mat))]

adj_graph <- inla.read.graph(Matrix::Matrix(mat, sparse = TRUE))

count_data <- count_data %>%
  filter(conflict == 1) %>%          # your intentional filter — keep it
  mutate(
    # Age index — still needed to avoid biasing coefficients
    age_idx    = as.integer(factor(age)),
    region_idx = as.integer(factor(region)),
    years_idx  = as.integer(factor(years)),
    urban      = as.integer(strata == "Urban"))

count_data$aid_interaction <- count_data$hum_scaled_lag * count_data$intense  

count_data <- count_data %>%
  mutate(deaths_scaled = scale(deaths) %>% as.numeric())

count_data$region.id.copy <- count_data$region_idx


out_dir <- "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/"
dir.create(out_dir, showWarnings = FALSE)

models <- list(
  model1 = model1,
  model2 = model2
)

formulas <- list(
  model1 = Y ~ 1 +
    f(age_idx,
      model       = "rw1",
      scale.model = TRUE,
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(3, 0.01)))) +
    f(region_idx,
      model       = "bym2",
      graph       = adj_graph,
      scale.model = TRUE,
      hyper       = list(
        prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
        phi  = list(prior = "pc",      param = c(0.7, 0.1))
      )) +
    f(years_idx,
      model       = "rw1",
      scale.model = TRUE,
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(3, 0.01)))) +
    f(region.id.copy,
      model       = "iid",
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(1, 0.5)))) +
    deaths_scaled +   
    hum_scaled_lag +        
   # intense +
    #aid_interaction +
    strata , 
  
  model2 =  Y ~ 1 +
    f(age_idx,
      model       = "rw1",
      scale.model = TRUE,
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(3, 0.01)))) +
    f(region_idx,
      model       = "bym2",
      graph       = adj_graph,
      scale.model = TRUE,
      hyper       = list(
        prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
        phi  = list(prior = "pc",      param = c(0.7, 0.1))
      )) +
    f(years_idx,
      model       = "rw1",
      scale.model = TRUE,
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(3, 0.01)))) +
    f(region.id.copy,
      model       = "iid",
      hyper       = list(prec = list(prior = "pc.prec",
                                     param = c(1, 0.5)))) +
    deaths_scaled +   
    hum_scaled_lag +        
    intense +
    aid_interaction +
    strata 
)

 ---- Loop over models ----
for (model_name in names(models)) {
  
  model   <- models[[model_name]]
  formula <- formulas[[model_name]]
  cat("\nProcessing:", model_name, "\n")
  
# The data prep to combine fitted values 
  fitted_mean_vec  <- model$summary.fitted.values$mean
  fitted_lower_vec <- model$summary.fitted.values$`0.025quant`
  fitted_upper_vec <- model$summary.fitted.values$`0.975quant`
  
  interval_data <- count_data %>%
    mutate(
      fitted_h = fitted_mean_vec,
      lower_h  = fitted_lower_vec,
      upper_h  = fitted_upper_vec,
      observed = Y / total
    ) %>%
    group_by(age_idx, years) %>%
    summarise(
      fitted   = mean(fitted_h),
      lower    = mean(lower_h),
      upper    = mean(upper_h),
      observed = mean(observed, na.rm = TRUE),
      .groups  = "drop"
    )
  
# Fitted vs actuall plot 
  plot_fitted <- ggplot(interval_data,
                        aes(x = observed, y = fitted)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_abline(intercept = 0, slope = 1,
                color = "red", linetype = "dashed") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  alpha = 0.1, width = 0) +
    labs(title = paste("Estimated vs Actual Mortality —", model_name),
         x     = "Observed mortality rate per cohort-year",
         y     = "Fitted mortality rate per cohort-year (mean + 95% CI)") +
    theme_minimal()
  
  ggsave(paste0(out_dir, model_name, "_fitted_vs_actual.png"),
         plot = plot_fitted, width = 10, height = 6)
  
# Credibility intervalls over time 
  ci_width_by_year <- interval_data %>%
    group_by(years) %>%
    summarise(
      ci_width = mean(upper - lower),
      .groups  = "drop"
    )
  
  plot_ci <- ggplot(ci_width_by_year,
                    aes(x = years, y = ci_width, group = 1)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Average Credible Interval Width Over Time —", model_name),
         x     = "Year",
         y     = "Mean CI width (mortality rate scale)") +
    theme_minimal()
  
  ggsave(paste0(out_dir, model_name, "_ci_width.png"),
         plot = plot_ci, width = 10, height = 6)
  
# The forest plot 
  fixed       <- model$summary.fixed
  fixed$param <- rownames(fixed)
  fixed       <- fixed[fixed$param != "(Intercept)", ]
  
  param_labels <- c(
    "deaths_scaled"   = "Conflict Deaths (scaled)",
    "hum_scaled_lag"  = "Humanitarian Aid Lag (scaled)",
    "strata"          = "Urban Strata",
    "intense"         = "High Violence",
    "aid_interaction" = "Hum. Aid × High Violence",
    "deaths_scaled_lag" = "Lag Conflict Deaths"
  )
  
  plot_forest <- ggplot(fixed, aes(x = mean, y = param)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = `0.025quant`, xmax = `0.975quant`),
                   height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_y_discrete(labels = param_labels) +
    labs(x     = "Posterior Mean (log-odds scale)",
         y     = NULL,
         title = paste("Fixed Effects —", model_name)) +
    theme_minimal()
  
  ggsave(paste0(out_dir, model_name, "_forest_plot.png"),
         plot = plot_forest, width = 10, height = 4)
  
# The prior predictive check
  cat("  Simulating prior predictive...\n")
  
  n_sim                <- 10000
  intercept_prior_mean <- -6
  intercept_prior_sd   <- sqrt(1 / 1.5)   #
  prior_samples <- plogis(
    rnorm(n_sim,
          mean = intercept_prior_mean,
          sd   = sqrt(intercept_prior_sd^2 + random_effect_sd^2))
  )
  
  
  observed_rates <- count_data %>%
    mutate(obs_rate = Y / total) %>%
    pull(obs_rate)
  
  
  plot_prior_df <- data.frame(
    value = c(observed_rates, prior_samples),
    type  = c(rep("Observed", length(observed_rates)),
              rep("Prior predictive", n_sim))
  )
  
  plot_prior_density <- ggplot(plot_prior_df, aes(x = value, fill = type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Observed"         = "steelblue",
                                 "Prior predictive" = "orange")) +
    coord_cartesian(xlim = c(0, 0.15)) +
    labs(title = paste("Prior Predictive Check —", model_name),
         x     = "Cohort-level mortality rate",
         y     = "Density",
         fill  = NULL) +
    theme_minimal()
  
  ggsave(paste0(out_dir, model_name, "_prior_predictive_density.png"),
         plot = plot_prior_density, width = 10, height = 6)
  
# The model table 
  fixed_all <- model$summary.fixed
  
  fixed_table <- data.frame(
    Parameter = rownames(fixed_all),
    Mean      = round(fixed_all[, "mean"], 3),
    SD        = round(fixed_all[, "sd"], 3),
    Q2.5      = round(fixed_all[, "0.025quant"], 3),
    Q97.5     = round(fixed_all[, "0.975quant"], 3),
    OR        = c(NA, round(exp(fixed_all[-1, "mean"]), 3)),
    OR_low    = c(NA, round(exp(fixed_all[-1, "0.025quant"]), 3)),
    OR_high   = c(NA, round(exp(fixed_all[-1, "0.975quant"]), 3))
  ) %>%
    mutate(
      `OR [95% CI]` = ifelse(is.na(OR), "—",
                             paste0(OR, " [", OR_low, ", ", OR_high, "]"))
    ) %>%
    select(Parameter, Mean, SD, Q2.5, Q97.5, `OR [95% CI]`)
  
  colnames(fixed_table) <- c("Parameter", "Mean", "SD",
                             "2.5%", "97.5%", "OR [95% CI]")
  
  # sig_rows computed from fixed_table directly (fit_table removed)
  sig_rows <- which(
    fixed_table[["2.5%"]]  > 0 |
      fixed_table[["97.5%"]] < 0
  )
  
  n_fixed <- nrow(fixed_table)
  
  tbl <- fixed_table %>%
    kbl(align    = c("l", "c", "c", "c", "c", "c"),
        format   = "latex",
        booktabs = TRUE,
        caption  = paste("Posterior summaries of fixed effects —",
                         model_name)) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(sig_rows, bold = TRUE) %>%
    pack_rows("Fixed Effects", 1, n_fixed) %>%
    footnote(
      general       = "Bold rows indicate 95% credible interval excludes null.",
      general_title = "Note:",
      escape        = FALSE
    )
  
  save_kable(tbl, paste0(out_dir, model_name, "_fixed_effects_table.tex"))
  cat("  Saved all outputs for", model_name, "\n")
}

cat("\nDone! All outputs saved to:", out_dir, "\n")

################################################################################
################################################################################


#---- Morans I test

nb    <- mat2listw(mat, style = "W")$neighbours
listw <- nb2listw(nb, style = "W")


region_data <- count_data %>%
  group_by(region_idx) %>%
  summarise(
    Y_sum     = sum(Y,     na.rm = TRUE),
    total_sum = sum(total, na.rm = TRUE)
  ) %>%
  mutate(rate = Y_sum / total_sum)

region_data <- region_data[order(region_data$region_idx), ]


moran_global <- moran.test(
  region_data$rate,
  listw       = listw,
  randomisation = TRUE,    
  alternative = "greater" 
)

print(moran_global)

################################################################################
################################################################################

#---- Leroux Test 

W <- mat          
D <- Diagonal(x = rowSums(W))        
Q <- D - W                            


n <- nrow(Q)
I <- Diagonal(n)                      
C <- I - Q


C_sparse <- inla.as.sparse(C)


model_leroux <- Y ~ 1 +
  deaths_scaled +
  hum_scaled_lag +
  strata +
  f(region_idx,
    model  = "generic1",
    Cmatrix = C_sparse,
    hyper  = list(
      beta  = list(prior = "logitbeta", param = c(1, 1)),   # prior on lambda
      theta = list(prior = "pc.prec",   param = c(0.5, 0.01)) # prior on precision
    ))


fit_leroux <- inla(
  model_leroux,
  family            = "betabinomial",
  data              = count_data,
  Ntrials           = count_data$total,
  verbose           = TRUE,
  control.mode      = list(restart = TRUE),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.predictor = list(compute = TRUE, link = 1),
  control.fixed     = list(
    mean.intercept = -6,
    prec.intercept = 1.5,
    mean           = 0,
    prec           = 0.1
  )
)


summary(fit_leroux)



lambda_marginal <- fit_leroux$marginals.hyperpar[["Beta for region_idx"]]
inla.zmarginal(lambda_marginal)

# Plot posterior of lambda
lambda <- plot(inla.smarginal(lambda_marginal),
     type = "l",
     xlab = expression(lambda),
     ylab = "Density",
     main = "Posterior of spatial mixing parameter")

################################################################################
################################################################################
#Creating the simulated results for aid and vioelcne

#
rm(list = ls())
count_data <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/used_data.rds")  # adjust path
model <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_1_final.rds")

# ---- extract scaling parameters ----
deaths_mean <- mean(count_data$deaths, na.rm = TRUE)
deaths_sd   <- sd(count_data$deaths, na.rm = TRUE)

aid_mean <- mean(count_data$hum_lag, na.rm = TRUE)
aid_sd   <- sd(count_data$hum_lag, na.rm = TRUE)


death_unscaled <- seq(0, 1000, by = 10)
aid_unscaled  <- seq(0, 100000000, by = 1000000)

# scale vectors using original scaling parameters 
death_scaled_lag <- (death_unscaled - deaths_mean) / deaths_sd
aid_scaled   <- (aid_unscaled - aid_mean) / aid_sd

#  extract fixed effects 
intercept   <- model$summary.fixed["(Intercept)", "mean"]
beta_deaths <- model$summary.fixed["deaths_scaled_lag", "mean"]
beta_aid    <- model$summary.fixed["hum_scaled_lag", "mean"]
beta_strata <- model$summary.fixed["strataurban", "mean"]

intercept_low   <- model$summary.fixed["(Intercept)", "0.025quant"]
beta_deaths_low <- model$summary.fixed["deaths_scaled_lag", "0.025quant"]
beta_aid_low    <- model$summary.fixed["hum_scaled_lag", "0.025quant"]

intercept_high   <- model$summary.fixed["(Intercept)", "0.975quant"]
beta_deaths_high <- model$summary.fixed["deaths_scaled_lag", "0.975quant"]
beta_aid_high    <- model$summary.fixed["hum_scaled_lag", "0.975quant"]

#filter to chosen region and year 
chosen_region <- "Amhara"
chosen_year   <- 2000

region_data <- count_data |> filter(region == chosen_region & years == chosen_year)


spatial_effect  <- model$summary.random$region_idx[unique(region_data$region_idx), "mean"]
temporal_effect <- model$summary.random$years_idx[unique(region_data$years_idx), "mean"]
cohort_effect   <- model$summary.random$age_idx[region_data$age_idx, "mean"]
iid_effect      <- model$summary.random$region.id.copy[unique(region_data$region_idx), "mean"]

# ---- add random effects to region data ----
region_data <- region_data |>
  mutate(
    spatial_re  = spatial_effect,
    temporal_re = temporal_effect,
    cohort_re   = cohort_effect,
    iid_re      = iid_effect
  )

region_data <- region_data |>
  mutate(
    spatial_re  = spatial_effect,
    temporal_re = temporal_effect,
    cohort_re   = cohort_effect,
    iid_re      = iid_effect
  )


mean_deaths_scaled <- (mean(count_data$deaths, na.rm = TRUE) - deaths_mean) / deaths_sd
mean_aid_scaled    <- (mean(count_data$hum_lag, na.rm = TRUE) - aid_mean) / aid_sd

# ---- u5mr computation function ----
compute_u5mr <- function(pred_data, x_var) {
  pred_data %>%
    group_by(age_idx, {{ x_var }}) %>%
    summarise(
      pred_mean  = mean(pred_mean),
      pred_lower = mean(pred_lower),
      pred_upper = mean(pred_upper),
      .groups = "drop"
    ) %>%
    mutate(z = case_when(
      age_idx == 1 ~ 1,
      age_idx == 2 ~ 11,
      age_idx == 3 ~ 12,
      age_idx == 4 ~ 12,
      age_idx == 5 ~ 12,
      age_idx == 6 ~ 12
    )) %>%
    group_by({{ x_var }}) %>%
    summarise(
      U5MR       = 1 - prod((1 - pred_mean)^z),
      U5MR_lower = 1 - prod((1 - pred_lower)^z),
      U5MR_upper = 1 - prod((1 - pred_upper)^z),
      .groups = "drop"
    )
}


death_pred_data <- map_dfr(seq_along(death_scaled_lag), function(i) {
  region_data |>
    mutate(
      death_unscaled = death_unscaled[i],
      pred_mean = plogis(
        intercept +
          beta_deaths * death_scaled_lag[i] +
          beta_aid    * mean_aid_scaled +
          beta_strata * as.numeric(strata == "urban") +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_lower = plogis(
        intercept_low +
          beta_deaths_low * death_scaled_lag[i] +
          beta_aid_low    * mean_aid_scaled +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_upper = plogis(
        intercept_high +
          beta_deaths_high * death_scaled_lag[i] +
          beta_aid_high    * mean_aid_scaled +
          spatial_re + temporal_re + cohort_re + iid_re
      )
    )
})

count_death_pred_data <- compute_u5mr(death_pred_data, death_unscaled)


aid_pred_data <- map_dfr(seq_along(aid_scaled), function(i) {
  region_data |>
    mutate(
      aid_unscaled = aid_unscaled[i],
      pred_mean = plogis(
        intercept +
          beta_deaths * mean_deaths_scaled +
          beta_aid    * aid_scaled[i] +
          beta_strata * as.numeric(strata == "urban") +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_lower = plogis(
        intercept_low +
          beta_deaths_low * mean_deaths_scaled +
          beta_aid_low    * aid_scaled[i] +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_upper = plogis(
        intercept_high +
          beta_deaths_high * mean_deaths_scaled +
          beta_aid_high    * aid_scaled[i] +
          spatial_re + temporal_re + cohort_re + iid_re
      )
    )
})

count_aid_pred_data <- compute_u5mr(aid_pred_data, aid_unscaled)


death_plot <- ggplot(count_death_pred_data, aes(x = death_unscaled, y = U5MR)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = U5MR_lower, ymax = U5MR_upper), alpha = 0.2) +
  coord_cartesian(ylim = range(c(count_death_pred_data$U5MR_lower,
                                 count_death_pred_data$U5MR_upper)) * c(0.9, 1.1)) +
  labs(
    title = "Simulated Mortality rates based on different levels of lagged Violence",
    x = "Number of Battle related Deaths in the previous year",
    y = "Predicted U5MR"
  ) +
  theme_minimal()

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/death_pred.jpg",
       plot = death_plot, width = 10, height = 5)


aid_plot <- ggplot(count_aid_pred_data, aes(x = aid_unscaled/1000000, y = U5MR)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = U5MR_lower, ymax = U5MR_upper), alpha = 0.2) +
  coord_cartesian(ylim = range(c(count_aid_pred_data$U5MR_lower,
                                 count_aid_pred_data$U5MR_upper)) * c(0.9, 1.1)) +
  labs(
    title = "Simulated Mortality rates based on different levels of Humanitarian Aid",
    x = "Humanitarian Aid in million USD",
    y = "Predicted U5MR"
  ) +
  theme_minimal()

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/aid_pred.jpg",
       plot = aid_plot, width = 10, height = 5)


# ---- load model 2 ----
model2 <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_2_final.rds")  # adjust path


intercept2       <- model2$summary.fixed["(Intercept)", "mean"]
beta_aid2        <- model2$summary.fixed["hum_scaled_lag", "mean"]
beta_intense2    <- model2$summary.fixed["intense", "mean"]
beta_aid_int2    <- model2$summary.fixed["aid_interaction", "mean"]
beta_strata2     <- model2$summary.fixed["strataurban", "mean"]

intercept2_low      <- model2$summary.fixed["(Intercept)", "0.025quant"]
beta_aid2_low       <- model2$summary.fixed["hum_scaled_lag", "0.025quant"]
beta_intense2_low   <- model2$summary.fixed["intense", "0.025quant"]
beta_aid_int2_low   <- model2$summary.fixed["aid_interaction", "0.025quant"]

intercept2_high     <- model2$summary.fixed["(Intercept)", "0.975quant"]
beta_aid2_high      <- model2$summary.fixed["hum_scaled_lag", "0.975quant"]
beta_intense2_high  <- model2$summary.fixed["intense", "0.975quant"]
beta_aid_int2_high  <- model2$summary.fixed["aid_interaction", "0.975quant"]


spatial_effect2  <- model2$summary.random$region_idx[unique(region_data$region_idx), "mean"]
temporal_effect2 <- model2$summary.random$years_idx[unique(region_data$years_idx), "mean"]
cohort_effect2   <- model2$summary.random$age_idx[region_data$age_idx, "mean"]
iid_effect2      <- model2$summary.random$region.id.copy[unique(region_data$region_idx), "mean"]

region_data2 <- region_data |>
  mutate(
    spatial_re  = spatial_effect2,
    temporal_re = temporal_effect2,
    cohort_re   = cohort_effect2,
    iid_re      = iid_effect2
  )


aid_pred_low <- map_dfr(seq_along(aid_scaled), function(i) {
  region_data2 |>
    mutate(
      aid_unscaled = aid_unscaled[i],
      pred_mean = plogis(
        intercept2 +
          beta_aid2       * aid_scaled[i] +
          beta_intense2   * 0 +
          beta_aid_int2   * (aid_scaled[i] * 0) +
          beta_strata2    * as.numeric(strata == "urban") +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_lower = plogis(
        intercept2_low +
          beta_aid2_low     * aid_scaled[i] +
          beta_intense2_low * 0 +
          beta_aid_int2_low * (aid_scaled[i] * 0) +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_upper = plogis(
        intercept2_high +
          beta_aid2_high     * aid_scaled[i] +
          beta_intense2_high * 0 +
          beta_aid_int2_high * (aid_scaled[i] * 0) +
          spatial_re + temporal_re + cohort_re + iid_re
      )
    )
})

u5mr_low <- compute_u5mr(aid_pred_low, aid_unscaled) |>
  mutate(conflict = "Low Violence")


aid_pred_high <- map_dfr(seq_along(aid_scaled), function(i) {
  region_data2 |>
    mutate(
      aid_unscaled = aid_unscaled[i],
      pred_mean = plogis(
        intercept2 +
          beta_aid2       * aid_scaled[i] +
          beta_intense2   * 1 +
          beta_aid_int2   * (aid_scaled[i] * 1) +
          beta_strata2    * as.numeric(strata == "urban") +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_lower = plogis(
        intercept2_low +
          beta_aid2_low     * aid_scaled[i] +
          beta_intense2_low * 1 +
          beta_aid_int2_low * (aid_scaled[i] * 1) +
          spatial_re + temporal_re + cohort_re + iid_re
      ),
      pred_upper = plogis(
        intercept2_high +
          beta_aid2_high     * aid_scaled[i] +
          beta_intense2_high * 1 +
          beta_aid_int2_high * (aid_scaled[i] * 1) +
          spatial_re + temporal_re + cohort_re + iid_re
      )
    )
})

u5mr_high <- compute_u5mr(aid_pred_high, aid_unscaled) |>
  mutate(conflict = "High Violence")


combined_pred <- bind_rows(u5mr_low, u5mr_high)

conflict_plot <- ggplot(combined_pred, aes(x = aid_unscaled / 1000000, y = U5MR,
                                           color = conflict, fill = conflict)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = U5MR_lower, ymax = U5MR_upper), alpha = 0.2, color = NA) +
  coord_cartesian(ylim = range(c(combined_pred$U5MR_lower,
                                 combined_pred$U5MR_upper)) * c(0.9, 1.1)) +
  scale_color_manual(values = c("Low Violence"  = "steelblue",
                                "High Violence" = "firebrick")) +
  scale_fill_manual(values  = c("Low Violence"  = "steelblue",
                                "High Violence" = "firebrick")) +
  labs(
    title  = "Predicted U5MR by Humanitarian Aid level under Low vs High Violence",
    x      = "Humanitarian Aid in million USD",
    y      = "Predicted U5MR",
    color  = NULL,
    fill   = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/conflict_aid_pred.jpg",
       plot = conflict_plot, width = 10, height = 5)
#################################################################################
################################################################################
#Density plots for main variables 

count_data <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/used_data.rds")

test_data <- count_data %>% filter(age_idx == 3)

main_density <- ggplot(count_data, aes(x = Y/total)) +
  geom_density(fill = "steelblue", alpha = 0.4, linewidth = 0.8) +
  geom_rug(alpha = 0.3) +                          
  geom_vline(aes(xintercept = mean(Y/total)), 
             linetype = "dashed", color = "red") +
  xlim(0, 0.2) +  
  theme_minimal() +
  labs(title = "Density of age cohort mortality rates", x = "Mortality", y = "Density")

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/main_density.jpg",
       plot = main_density, width = 10, height = 5)

aid_test <- count_data %>% group_by(years, region) %>% 
  summarise(aid = mean(hum_aid))

aid_density <- ggplot(aid_test, aes(x = aid/1000000)) +
  geom_density(fill = "steelblue", alpha = 0.4, linewidth = 0.8) +
  geom_rug(alpha = 0.3) +                          
  geom_vline(aes(xintercept = mean(aid)/1000000), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Density of the humanitarian aid data", x = "Humanitarian aid in million USD", y = "Density")

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/aid_density.jpg",
       plot = aid_density, width = 10, height = 5)

death_test <- count_data %>% group_by(years, region) %>% 
  summarise(death = mean(deaths))

death_density <- ggplot(death_test, aes(x = death/1000)) +
  geom_density(fill = "steelblue", alpha = 0.4, linewidth = 0.8) +
  geom_rug(alpha = 0.3) +                          
  geom_vline(aes(xintercept = mean(death)/1000), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Density of battle related deaths per region year", x = "Battle related deaths in thousands", y = "Density")

death_density

ggsave("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Graphs/deah_density.jpg",
       plot = death_density, width = 10, height = 5)

#################################################################################
#################################################################################
# Summary statistics table 

count_data <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/used_data.rds")

panel_a <- count_data %>%
  mutate(mortality_rate = Y / total) %>%
  select(mortality_rate) %>%
  summarise(across(everything(), list(
    Mean   = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD     = ~sd(., na.rm = TRUE),
    Min    = ~min(., na.rm = TRUE),
    Max    = ~max(., na.rm = TRUE),
    N      = ~sum(!is.na(.))
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", "Statistic"),
               names_pattern = "^(.*)_(Mean|Median|SD|Min|Max|N)$",
               values_to = "Value") %>%
  pivot_wider(names_from  = "Statistic",
              values_from = "Value") %>%
  mutate(Variable = c("Mortality Rate (Y/total)"))

panel_b <- count_data %>%
  select(hum_aid, deaths, hum_scaled, deaths_scaled) %>%
  distinct() %>%
  summarise(across(everything(), list(
    Mean   = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD     = ~sd(., na.rm = TRUE),
    Min    = ~min(., na.rm = TRUE),
    Max    = ~max(., na.rm = TRUE),
    N      = ~sum(!is.na(.))
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", "Statistic"),
               names_pattern = "^(.*)_(Mean|Median|SD|Min|Max|N)$",
               values_to = "Value") %>%
  pivot_wider(names_from  = "Statistic",
              values_from = "Value") %>%
  mutate(Variable = c("Humanitarian Aid", "Battle Deaths",
                      "Humanitarian Aid (Scaled)", "Battle Deaths (Scaled)"))


bind_rows(panel_a, panel_b) %>%
  kbl(digits   = 3,
      caption  = "Descriptive Statistics",
      format   = "latex",
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  pack_rows("Panel A: Observation-Level Variables", 1, nrow(panel_a)) %>%
  pack_rows("Panel B: Region-Year Level Variables", nrow(panel_a) + 1,
            nrow(panel_a) + nrow(panel_b)) %>%
  footnote(general = "N refers to non-missing observations. Region-year variables are computed on distinct region-year observations.")
library(modelsummary)

readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/glm_model_final.rds")

library(modelsummary)

modelsummary(
  glm_model,
  
  output = "latex_tabular",
  
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  
  exponentiate = TRUE,
  
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  
  fmt = 3,
  
  coef_rename = c(
    "Y_lag"             = "Spatial Lag (U5MR)",
    "deaths_scaled"     = "Conflict Deaths (scaled)",
    "deaths_scaled_lag" = "Conflict Deaths Lag (scaled)",
    "hum_scaled_lag"    = "Humanitarian Aid Lag (scaled)",
    "strataUrban"       = "Urban Strata"
  ),
  
  coef_omit = "region_fe|time_fe|age_fe",
  
  gof_map = c("nobs", "aic", "bic"),
  
  title = "Logistic Regression -- Fixed Effects Model",
  
  notes = c(
    "* p < 0.1, ** p < 0.05, *** p < 0.01",
    "Odds ratios reported.",
    "Region, time, and age fixed effects included but not shown."
  ),
  
  escape = FALSE
)

# Robustness check table 
model_rob_mort <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_robust_m_final.rds")
model_rob_aid <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_robust_h_final.rds")
model_rob_con <- readRDS("C:/Users/fabia/OneDrive/Desktop/Saved outputs/model_robust_c_final.rds")


extract_fixed <- function(model, model_name) {
  fixed_all <- model$summary.fixed
  
  data.frame(
    Model     = model_name,
    Parameter = rownames(fixed_all),
    Mean      = round(fixed_all[, "mean"], 3),
    SD        = round(fixed_all[, "sd"], 3),
    Q2.5      = round(fixed_all[, "0.025quant"], 3),
    Q97.5     = round(fixed_all[, "0.975quant"], 3),
    OR        = c(NA, round(exp(fixed_all[-1, "mean"]), 3)),
    OR_low    = c(NA, round(exp(fixed_all[-1, "0.025quant"]), 3)),
    OR_high   = c(NA, round(exp(fixed_all[-1, "0.975quant"]), 3))
  ) %>%
    mutate(
      `OR [95% CI]` = ifelse(is.na(OR), "—",
                             paste0(OR, " [", OR_low, ", ", OR_high, "]"))
    ) %>%
    select(Model, Parameter, Mean, SD, Q2.5, Q97.5, `OR [95% CI]`)
}


rob1 <- extract_fixed(model_rob_mort, "No Mortality Outliers")
rob2 <- extract_fixed(model_rob_aid,       "No Aid Outliers")
rob3 <- extract_fixed(model_rob_con,  "No Violence Outliers")


combined_table <- bind_rows(rob1, rob2, rob3)

colnames(combined_table) <- c("Model", "Parameter", "Mean", "SD",
                              "2.5%", "97.5%", "OR [95% CI]")

combined_table %>%
  select(-Model) %>%  
  kable(format    = "latex",
        booktabs  = TRUE,
        caption   = "Posterior summaries of fixed effects — robustness checks",
        label     = "tab:robustness") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  pack_rows("No Mortality Outliers", 
            which(combined_table$Model == "No Mortality Outliers")[1],
            tail(which(combined_table$Model == "No Mortality Outliers"), 1)) %>%
  pack_rows("No Aid Outliers",
            which(combined_table$Model == "No Aid Outliers")[1],
            tail(which(combined_table$Model == "No Aid Outliers"), 1)) %>%
  pack_rows("No Violence Outliers",
            which(combined_table$Model == "No Violence Outliers")[1],
            tail(which(combined_table$Model == "No Violence Outliers"), 1))%>%
  column_spec(1, bold = TRUE)

