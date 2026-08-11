## load packages
require(pals)
require(tidyverse)
require(ggh4x)
require(grid)
require(magick)
library(ggdist)
library(forcats)
library(cowplot)
library(ggridges)


source("01_data_management.R")
## colors for the six models: 
colors_model = c(stepped2(2)[2], stepped(14)[14], stepped2(20)[15], stepped3(7)[6], stepped3(11)[9], stepped2(11)[10])
alpha_values= c(0.3, 0.35, 0.3, 0.35, 0.3, 0.35)

## colors for the sensitivity analysis
colors_fswassumption = c("gray30", 
                         "gray30", 
                         "#E7298A", "#FF7F00" )


########################################################################
## Table: Model parameters (compare prior and posterior distribution) ##
######################################################################## 

tbl_modelparam = modelparameters_summary %>% 
  rowwise() %>% 
  mutate(text =  
           case_when(name %in% c( "Mean_gamma_sex_activity",  "SD_gamma_sex_activity") ~ 
                       paste0(sprintf("%.1f", mean), " (", sprintf("%.1f", q0025), "-", sprintf("%.1f", q0975), ")"), 
                     name %in% c("FSW_contact_Hrmen","FSW_start_epi", "RR_fertility") ~ 
                       paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", q0025), "-", sprintf("%.2f", q0975), ")"), 
                     name %in% c("Assortativeness_mixing","HIV_diag_entrySW"  ) ~
                       paste0(sprintf("%.3f", mean), " (", sprintf("%.3f", q0025), "-", sprintf("%.3f", q0975), ")"),
                     name %in% c( "Client_FSW_transmission",  "F_M_transmission_ST", "M_F_transsmission_ST" ) ~ 
                       paste0(sprintf("%.4f", mean), " (", sprintf("%.4f", q0025), "-", sprintf("%.4f", q0975), ")"), 
                     name == "HIV_prevalence_init_HR" ~
                       paste0(sprintf("%.3f", mean), "% (", sprintf("%.3f", q0025), "-", sprintf("%.3f", q0975), "%)"))) %>% 
  select(scenario, name, text) %>% 
  pivot_wider(values_from = text, names_from = scenario)

tbl_modelparam = tbl_modelparam %>% 
  mutate(name = factor(name, levels = c("Mean_gamma_sex_activity", "SD_gamma_sex_activity", 
                                        "Assortativeness_mixing", 
                                        "F_M_transmission_ST","M_F_transsmission_ST", 
                                        "RR_fertility", "HIV_prevalence_init_HR", 
                                        "FSW_contact_Hrmen", "HIV_diag_entrySW", 
                                        "Client_FSW_transmission", "FSW_start_epi"))) %>% 
  arrange(name) %>% 
  rename(Parameter = name)

tbl_modelparam


###########################################################
## Plot prior versus posteriors of calibrated paremeters ##
########################################################### 

set.seed(1)
n_draws = 10000

# Simualte priors

prior_draws = bind_rows(
  tibble(name = "Mean_gamma_sex_activity", value = rgamma(n_draws, 49.0, 1.40)),
  tibble(name = "SD_gamma_sex_activity", value = rgamma(n_draws, 121.0, 5.5)),
  tibble(name = "Assortativeness_mixing", value = rbeta(n_draws, 5.80, 3.87)),
  tibble(name = "F_M_transmission_ST", value = rbeta(n_draws, 7.05, 874)),
  tibble(name = "M_F_transsmission_ST", value = rbeta(n_draws, 5.68, 468)),
  tibble(name = "RR_fertility", value = rgamma(n_draws, 100, 76.9)),
  tibble(name = "HIV_prevalence_init_HR", value = 100*runif(n_draws, 0, 0.001)),
  tibble(name = "FSW_contact_Hrmen", value = rgamma(n_draws, 5.444, 1.555)),
  tibble(name = "HIV_diag_entrySW", value = runif(n_draws, 0, 1)),
  tibble(name = "Client_FSW_transmission", value = rbeta(n_draws, 3.995, 3991)),
  tibble(name = "FSW_start_epi",  value = rgamma(n_draws, 6.25, 1.25))
) %>%
  mutate(scenario = "Prior")

modelparameters_post = modelparameters_post %>% mutate(
  value = ifelse(name == "HIV_prevalence_init_HR", 0.1*value, value))

param_data = bind_rows(modelparameters_post, prior_draws)

scen_levels = c("Scenario 3b","Scenario 3a","Scenario 2b","Scenario 2a","Scenario 1b","Scenario 1a","Prior")
param_data = param_data %>%
  mutate(scenario = factor(scenario, levels = scen_levels))

param_labels = c(
  "Mean_gamma_sex_activity" = "Mean partnership formation",
  "SD_gamma_sex_activity"  = "SD partnership formation",
  "Assortativeness_mixing" = "Sexual mixing parameter",
  "F_M_transmission_ST" = "Female-to-male transmission",
  "M_F_transsmission_ST" = "Male-to-female transmission",
  "RR_fertility" = "RR HIV+ fertility",
  "HIV_prevalence_init_HR" = "Initial HIV prev. high-risk women (%)",
  "FSW_contact_Hrmen" = "Annual rate of FSW contact",
  "HIV_diag_entrySW" = "Reduction in SW entry after diagnosis",
  "Client_FSW_transmission" = "Client-to-FSW transmission (tau)",
  "FSW_start_epi" = "R (FSW start of epidemic)"
)


param_levels = c(
  "Mean_gamma_sex_activity",
  "SD_gamma_sex_activity" ,
  "Assortativeness_mixing",
  "F_M_transmission_ST" ,
  "M_F_transsmission_ST",
  "RR_fertility" ,
  "HIV_prevalence_init_HR" ,
  "FSW_contact_Hrmen",
  "HIV_diag_entrySW",
  "Client_FSW_transmission",
  "FSW_start_epi")


means_df = param_data %>%
  group_by(name, scenario) %>%
  summarise(mean = mean(value), .groups = "drop") %>%
  mutate(y = as.numeric(scenario))

param_data = param_data %>% 
  mutate(name = factor(name, levels = param_levels))

means_df = means_df %>% 
  mutate(name = factor(name, levels = param_levels))

p_calibparam <- ggplot(param_data, aes(x = value, y = scenario, fill = scenario)) +
  stat_halfeye(
    aes(),
    normalize = "xy", 
    height = 0.9,
    alpha = 0.6,
    point_interval = "mean_qi",
    .width = 0.95,
    slab_colour = "black",
    slab_linewidth = 0.3,
    interval_size = 3,
    point_size = 1.2,
    scale = 0.9) +
  geom_segment(
    data = means_df,
    aes(x = mean, xend = mean, y = y, yend = y + 0.55),
    colour = "grey15", linewidth = 0.4, inherit.aes = FALSE) +
  facet_wrap(~ name, scales = "free_x", ncol = 3,
             labeller = labeller(name = param_labels)) +
  scale_fill_manual(values = rev(c("grey70", colors_model)))+
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_calibparam


###################################
## Plot transmission assumption: ##
###################################

pfswchar = FSWchar_assumptions %>% ggplot(aes(y=mean, x = year))  +
  geom_line(aes(col = parameter))  +
  geom_ribbon(aes(ymin = q025, ymax =q075, fill = parameter), alpha = 0.3) + 
  geom_ribbon(aes(ymin = q0025, ymax= q0975, fill = parameter), alpha = 0.2)  +
  facet_nested(
    rows = vars(parameter),
    cols = vars(assumption, scenario),  
    scales = "free_y",
    switch = "y",
    nest_line = TRUE
  ) +
  theme_bw() +
  theme(
    ggh4x.facet.nestline = element_line(color = NA),
    strip.background.x = element_rect(fill = "grey90", colour = NA),
    strip.text.x = element_text(margin = margin(4, 6, 4, 6))
  ) +
  labs(y=NULL, x=NULL) +
  scale_color_manual(values = c("#E63946", "#F4A261" )) + 
  scale_fill_manual(values = c("#E63946", "#F4A261")) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_continuous(breaks = c(1985, 1996, 2019,  2045))

Trans_assumptions = Trans_assumptions %>% 
  mutate(assumption = factor(assumption, 
                             levels=c("Time-invariant (1: constant)", 
                                      "Time-varying (2: exponentially declining)", 
                                      "Time-varying (3: dynamically changing)"), 
                             labels=c("Time-invariant (1: constant)", 
                                      "Time-varying (2: exponentially declining)", 
                                      "Time-varying (3: exposure-dependent change)"))) 

ptrans = Trans_assumptions %>%
  ggplot(aes(y = mean_tr, x = year)) +
  geom_line(col = "#264653") +
  geom_ribbon(aes(ymin = q025_tr, ymax = q075_tr), fill = "#264653", alpha = 0.3) +
  geom_ribbon(aes(ymin = q0025_tr, ymax = q0975_tr), alpha = 0.2, fill = "#264653") +
  facet_nested(
    rows = vars(parameter),
    cols = vars(assumption, scenario),  
    scales = "free_y",
    switch = "y",
    nest_line = TRUE
  ) +
  theme_bw() +
  labs(y = NULL, x = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(
     ggh4x.facet.nestline = element_line(color = NA),
    strip.background.x = element_rect(fill = "grey90", colour = NA),
    strip.text.x = element_text(margin = margin(4, 6, 4, 6))
  )  + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_continuous(breaks = c(1985, 1996, 2019, 2045))


blank <- cowplot::ggdraw()   

ptemp = cowplot::plot_grid(blank,pfswchar,nrow = 1, labels = LETTERS, 
                           label_size = 14, label_fontface = "bold", ncol = 2, rel_widths = c(1.9,1.0))


cowplot::plot_grid(ptemp, ptrans, ncol = 1, labels = c("","C"), rel_heights = c(1.87, 1.1), 
                   label_size =  14, label_fontface = "bold")


## adding the schematic of the FSW component in Thembisa:

base <- theme_bw(base_size = 11, base_family = "Helvetica") +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(), 
        legend.position = "none")

param_labels <- as_labeller(c("Transmission risk" = "'Transmission risk'~beta*'('*italic(t)*')'"), label_parsed)

ptrans2 = Trans_assumptions %>%
  ggplot(aes(y = mean_tr, x = year)) +
  geom_line(col = "#264653") +
  geom_ribbon(aes(ymin = q025_tr, ymax = q075_tr), fill = "#264653", alpha = 0.3) +
  geom_ribbon(aes(ymin = q0025_tr, ymax = q0975_tr), alpha = 0.2, fill = "#264653") +
  facet_nested( rows = vars(parameter),
                cols = vars(assumption, scenario),   
                scales = "free_y",
                switch = "y",
                nest_line = TRUE, 
                labeller = labeller(parameter = param_labels)) +
  theme_bw() +
  labs(y = NULL, x = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme(ggh4x.facet.nestline = element_line(color = NA),
        strip.background.x = element_rect(fill = "grey90", colour = NA),
        strip.text.x = element_text(margin = margin(4, 6, 4, 6)))  + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_continuous(breaks = c(1985, 1996, 2019, 2045))

paramB_labels <- as_labeller(
  c("FSW age (years)" = "'FSW age'~italic(a)*'('*italic(t)*')'~'(years)'",
    "SW duration (years)" = "'SW duration'~1/lambda*'('*italic(t)*')'~'(years)'"), label_parsed)

pfswchar2 = FSWchar_assumptions %>% ggplot(aes(y=mean, x = year))  +
  geom_line(aes(col = parameter))  +
  geom_ribbon(aes(ymin = q025, ymax =q075, fill = parameter), alpha = 0.3) + 
  geom_ribbon(aes(ymin = q0025, ymax= q0975, fill = parameter), alpha = 0.2)  +
  facet_nested(rows = vars(parameter),
               cols = vars(assumption, scenario),  
               scales = "free_y",
               switch = "y",
               nest_line = TRUE, 
               labeller = labeller(parameter = paramB_labels, 
                                   assumption = label_wrap_gen(width = 18))) +
  theme_bw() +
  theme(ggh4x.facet.nestline = element_line(color = NA),
        strip.background.x = element_rect(fill = "grey90", colour = NA),
        strip.text.x = element_text(margin = margin(4, 6, 4, 6))
  ) +
  labs(y=NULL, x=NULL) +
  scale_color_manual(values = c("#E63946", "#F4A261" )) + 
  scale_fill_manual(values = c("#E63946", "#F4A261")) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_continuous(breaks = c(1985, 1996, 2019,  2045))


panelB <- pfswchar2 + theme(legend.position = "none") + base
panelC <- ptrans2 + base

schematic_img <- image_read_pdf("../data/model_schematic.pdf", density = 600)
schematic_img <- image_trim(schematic_img)                         
schematic_img <- image_border(schematic_img, "white", "60x60")    
panelA <- ggdraw() + draw_image(schematic_img) + 
  theme(panel.border  = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


top <- plot_grid(panelA, panelB,
                 labels = c("A", "B"),
                 label_size  = 14,
                 label_fontface = "bold",
                 ncol  = 2,
                 rel_widths  = c(1.9, 1.0))

final <- plot_grid(
  top, panelC,
  labels = c("", "C"),  
  label_size  = 14,
  label_fontface = "bold",
  ncol  = 1,
  rel_heights = c(1.87, 1.1))


final

#######################
## Plot Bayes Factor ##
#######################

heat_map = logLik %>% 
  mutate(choose= case_when(scenario_I == "3a" ~ 1, 
                           scenario_I == "3b" & fav!= "3a" ~ 1, 
                           scenario_I == "2a" & !fav %in% c("3b", "3a") ~ 1, 
                           scenario_I == "2b" & !fav %in% c("3b", "3a", "2a") ~ 1,
                           scenario_I == "1b" & !fav %in% c("3b", "3a", "2a", "2b") ~ 1, 
                           scenario_I == "1a" & scenario_II == "1a" ~1,
                           TRUE ~ 0)) %>% 
  filter(choose == 1) %>% 
  select(scenario_I, scenario_II, BF) %>% 
  mutate(BF = case_when(BF== 1 ~ NA_real_, 
                        TRUE ~ BF)) %>% 
  mutate(BF_band = cut(BF,
                       breaks = c(1, 3, 10, 30, 100, Inf),
                       right = FALSE,
                       labels = c("1–<3 (weak)",
                                  "3–<10 (moderate)",
                                  "10–<30 (strong)",
                                  "30–<100 (very strong)",
                                  "≥100 (decisive)")))

heat_map %>% 
  mutate(scenario_I = factor(scenario_I, levels = rev(c("3a", "3b", "2a", "2b", "1b", "1a"))), 
         scenario_II = factor(scenario_II, levels = (c("3a", "3b", "2a", "2b", "1b", "1a")))) %>% 
  ggplot(aes(x=scenario_II, y=scenario_I, fill = BF_band)) + 
  geom_tile(color="white") + 
  theme_bw() + 
  geom_text(aes(label = round(BF,1))) + 
  scale_fill_manual(
    name = "Evidence (Jeffreys)",
    breaks = c(
      "1–<3 (weak)",
      "3–<10 (moderate)",
      "10–<30 (strong)",
      "30–<100 (very strong)",
      "≥100 (decisive)"), 
    values = c(
      "1–<3 (weak)"          = "#e6f0ff",
      "3–<10 (moderate)"     = "#b3d1ff",
      "10–<30 (strong)"      = "#80b3ff",
      "30–<100 (very strong)"= "#4d94ff",
      "≥100 (decisive)"      = "#1a75ff"), 
    na.value = NA)  + 
  labs(x=NULL, y= NULL)


##############################################################
## Plot HIV incidence, prevalence and VL Suppression in FSW ## 
##############################################################

p_HIV_inFSW = data_tot2 %>% 
    filter(Year <=2025) %>% 
    mutate(model = gsub("model ", "", model)) %>% 
    mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                      TRUE ~ "b - increasing FSW age & SW duration")) %>% 
    mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                          "HIV prevalence in FSW", 
                                          "VL suppression in FSW"))) %>% 
    mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
    mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
    mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
    mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption)) %>% 
    ggplot(aes(x=Year, y=y)) + 
    theme_bw() + 
    geom_line(aes(col = model, linetype = FSW_assumption), size = 0.8) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = model, alpha = model)) +
    scale_color_manual(values = c(colors_model), name="Scenario") + 
    scale_fill_manual(values = c(colors_model), name="Scenario") + 
    scale_alpha_manual(values = alpha_values, name = "Scenario")+
    labs(y=NULL, x = NULL) + 
    scale_y_continuous(labels = scales::percent,  limits = c(0,1)) + 
    facet_grid(type ~ Transmission_assumption, scales = "free_y", switch = "y") + 
    theme(legend.position = "top") +  
    geom_pointrange(data = FSW_prev_data ,
                    aes(y = prev, ymin = tot_prev_lwr, ymax=tot_prev_upr, group = dodge_group,
                        shape = "Calibration data"),col= "gray70", size = 0.1, position = position_dodge(1)) +
    geom_pointrange(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                       "HIV prevalence in FSW", 
                                                                                       "VL suppression in FSW"))),
                    aes(y = est, ymin = lb, ymax = ub, x = year,
                        shape = shape), size = 0.3) +
    scale_shape_manual(values = c(1,8), breaks= c("Validation data", "Calibration data"), labels = c("Validation data", 
                                                                                                     "Calibration data"),  name = NULL) +    
    theme(legend.position = c(0.02, 0.95), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(1.2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 9), 
          strip.text = element_text(size = 10)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "horizontal", order = 1), 
           fill = guide_legend(direction = "horizontal", order = 1), 
           alpha = guide_legend(direction = "horizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)) 



# Panel B zoom into the year 2019: 
data_comp_valid = data_tot2 %>% 
  filter(Year == 2019)

data_comp_valid = data_comp_valid %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  mutate(facet = "  \n  ") %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) 


p_valid = data_comp_valid %>% 
    ggplot(aes(y=y, x = (Year))) + 
    geom_point(aes(group = model, col = model), position = position_dodge(0.3)) + 
    geom_pointrange(aes(group = model, col = model, ymin = ymin, ymax = ymax), position = position_dodge(0.3)) + 
    facet_grid(type~facet,scales = "free", switch= "y") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(y=NULL, x = NULL) + 
    geom_pointrange(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                       "HIV prevalence in FSW", 
                                                                                       "VL suppression in FSW"))), 
                    aes(y = est, ymin = lb, ymax = ub, x = (year), shape = "Validation\ndata")) +
    theme_bw() + 
    geom_rect(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                 "HIV prevalence in FSW", 
                                                                                 "VL suppression in FSW"))), 
              aes(xmin = -Inf, xmax = Inf, ymin = lb, ymax = ub,
                  fill = "Validation\ndata"),
              alpha = 0.4, inherit.aes = FALSE) + 
    scale_x_continuous(breaks = 2019) + 
    geom_hline(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                  "HIV prevalence in FSW", 
                                                                                  "VL suppression in FSW"))), 
               aes(yintercept = est, linetype = "Validation\ndata"), size = 0.3) +
    theme_minimal() + 
    guides(color = "none") +
    scale_linetype_manual(values = 2, name = NULL) + 
    scale_shape_manual(values = 1, name = NULL) + 
    scale_fill_manual(values = "grey70", name = NULL) + 
    scale_color_manual(values = colors_model, name = "Scenario") + 
    theme(legend.position = c(0.7, 0.95), 
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank()) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 9), 
          strip.text = element_text(size = 10)) + 
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          plot.background  = element_rect(fill = "white", colour = "white")) 

cowplot::plot_grid(p_HIV_inFSW, p_valid, ncol = 2, rel_widths = c(0.79, 0.21), labels = LETTERS)

#######################
## Plot PAF sex work ##
#######################

temp_data_correct = 
  PAF_CSW %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  filter(year >=2000) %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption))


p_paf = temp_data_correct %>% 
    mutate(facet_row = "Population Attributable Fraction (PAF) sex work") %>% 
    ggplot(aes(y=PAF_mean, x= year)) + 
    geom_rect(aes(xmin = 2026, xmax = 2045, ymin = 0, ymax = 1), 
              fill = "lightgray", alpha = 0.05, inherit.aes = FALSE) +
    geom_line(aes(col = model, linetype = FSW_assumption), size = 0.8) + 
    geom_ribbon(data = temp_data_correct %>% filter(year <=2025), 
                aes(ymin = PAF_q025, ymax = PAF_q975, fill = model, 
                    alpha = model)) +
    geom_ribbon(data = temp_data_correct %>% filter(year >2025), 
                aes(ymin = PAF_q025, ymax = PAF_q975, col = model),lty = 2, alpha = 0, 
                show.legend = FALSE) +    
    facet_grid(facet_row~Transmission_assumption, switch = "y") + 
    coord_cartesian(ylim = c(0,0.5)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() + 
    scale_color_manual(values = colors_model) + 
    scale_fill_manual(values = colors_model) + 
    scale_alpha_manual(values = alpha_values) + 
    labs(y=NULL, color = "Scenario", alpha = "Scenario",
         fill = "Scenario", x = NULL) + 
    scale_x_continuous(breaks = c(2000, 2005, 2015, 2025, 2035, 2045), limits = c(2000,2045)) + 
    scale_linetype_manual(values = c(3,1), name = NULL)  + 
    theme(legend.position =  c(0.02, 0.88), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 8), 
          strip.text = element_text(size = 9)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "horizontal", order = 1), 
           fill = guide_legend(direction = "horizontal", order = 1), 
           alpha = guide_legend(direction = "horizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


p_paf

## PAF sex work: distinguish between new infections in clients and in FSW 

temp_data_correct_clients = 
  PAF_clients %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  filter(year >=2000) %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption)) %>% 
  mutate(facet_row = "Proportion of new infecitons in clients") 



temp_data_correct_fsw = 
  PAF_fsw %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  filter(year >=2000) %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption)) %>% 
  mutate(facet_row = "Proportion of new infecitons in FSW") 


distribution_paf = temp_data_correct_clients %>%
  bind_rows(temp_data_correct_fsw) %>%
  mutate(facet_row = paste0(facet_row, "\n(among all new adult HIV infections)"))


(p_paf_separate = distribution_paf %>% 
       filter(year >=2000, year <= 2045) %>% 
       ggplot(aes(y=PAF_mean, x= year)) + 
       geom_rect(aes(xmin = 2026, xmax = 2045, ymin = 0, ymax = 0.5), 
                 fill = "lightgray", alpha = 0.05, inherit.aes = FALSE) +
       geom_line(aes(col = model, linetype = FSW_assumption), size = 0.8) + 
       geom_ribbon(data = distribution_paf %>% filter(year <=2025), 
                   aes(ymin = PAF_q025, ymax = PAF_q975, fill = model, alpha = model)) +
       geom_ribbon(data = distribution_paf %>% filter(year >2025), 
                   aes(ymin = PAF_q025, ymax = PAF_q975, col = model),lty = 2, alpha = 0, 
                   show.legend = FALSE) +    
       facet_grid(facet_row~Transmission_assumption, switch = "y", scales = "free") + 
       coord_cartesian(ylim = c(0,0.2)) +
       scale_y_continuous(labels = scales::percent) +
       theme_bw() + 
       scale_color_manual(values = colors_model) + 
       scale_fill_manual(values = colors_model) + 
       scale_alpha_manual(values = alpha_values)+ 
       labs(y=NULL, color = "Scenario", alpha = "Scenario",
            fill = "Scenario", x = NULL) + 
       scale_x_continuous(breaks = c(2000, 2005, 2015, 2025, 2035, 2045), limits = c(2000,2045)) + 
       scale_linetype_manual(values = c(3,1), name = NULL)  + 
       theme(legend.position =  c(0.02, 0.42), 
             legend.box = "horizontal",
             legend.justification="left",
             legend.margin=margin(-2,-2,-2,-2),
             legend.box.margin=margin(-2,-2,-2,-2), 
             legend.background = element_blank(), 
             legend.spacing.x = unit(2, "cm")) + 
       theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
             legend.text = element_text(size = 9), 
             legend.title = element_text(size =9), 
             axis.text = element_text(size = 8), 
             strip.text = element_text(size = 9)) + 
       scale_linetype_manual(values = c(3,1), name = NULL) + 
       guides(color = guide_legend(direction = "horizontal", order = 1), 
              fill = guide_legend(direction = "horizontal", order = 1), 
              alpha = guide_legend(direction = "horizontal", order = 1), 
              linetype = guide_legend(order = 2), 
              shape = guide_legend(order = 3)) + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1)))


# the same but on the relative scale:

relPAF = relPAF %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption))




(P_relpaf = relPAF %>% 
    ggplot(aes(y=share_mean, x= year)) + 
    geom_rect(aes(xmin = 2026, xmax = 2045, ymin = 0, ymax = 1), 
              fill = "lightgray", alpha = 0.05, inherit.aes = FALSE) +
    geom_line(aes(col = model, linetype = FSW_assumption), size = 0.8) + 
    geom_ribbon(data = relPAF %>% filter(year <=2025), 
                aes(ymin = share_q025, ymax = share_q975, fill = model, alpha = model)) +
    geom_ribbon(data = relPAF %>% filter(year >2025), 
                aes(ymin = share_q025, ymax = share_q975, col = model),lty = 2, alpha = 0, 
                show.legend = FALSE) +    
    facet_grid(.~Transmission_assumption, switch = "y") + 
    scale_y_continuous(labels = scales::percent) + 
    theme_minimal() + 
    scale_color_manual(values = colors_model) + 
    scale_fill_manual(values = colors_model) + 
    scale_alpha_manual(values =alpha_values) + 
    labs(y=NULL, color = "Scenario", alpha = "Scenario", 
         fill = "Scenario", x = NULL) + 
    scale_x_continuous(breaks = c(2000, 2015, 2025, 2035, 2045), limits = c(2000,2045)) + 
    scale_linetype_manual(values = c(3,1), name = NULL)  + 
    theme(legend.position =  c(0.02, 0.88), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 8), 
          strip.text = element_text(size = 9)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "horizontal", order = 1), 
           fill = guide_legend(direction = "horizontal", order = 1), 
           alpha = guide_legend(direction = "horizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)) + 
    labs(y="Share of sex-work-related infections\noccurring in FSW (vs. clients)") + 
    theme(panel.background = element_rect(fill = "white", colour = "white"),
          plot.background  = element_rect(fill = "white", colour = "white")))



cowplot::plot_grid(p_paf_separate + theme(legend.position = "bottom"), 
                   P_relpaf+theme(legend.position = "none"), nrow = 2, 
                   rel_heights = c(0.65, 0.35), labels = LETTERS)



###############
## Plot IRRs ##
###############

IRR_age = IRR_agematched %>% 
  filter(year <=2045) %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption))

data_jones = data.frame(y = 4.9, lb = 3.4, ub = 7.1)



(p_IRR_age = IRR_age %>%
    mutate(facet_row = "Age-standardized IRR (FSW/females gen pop)") %>% 
    ggplot(aes(y=IRR_mean, x= year)) + 
    geom_rect(data = data_jones, 
              aes(xmin = -Inf, xmax = Inf, ymin = lb, ymax = ub),
              alpha = 0.4,fill = "grey70",  inherit.aes = FALSE) + 
    geom_hline(yintercept = 4.9, lty = 2, col = "gray40") + 
    geom_line(aes(col = model, linetype = FSW_assumption), size = 0.8) + 
    geom_ribbon(data = IRR_age %>% filter(year <=2025), 
                aes(ymin = IRR_q025, ymax = IRR_q975, fill = model, alpha = model)) +
    geom_ribbon(data = IRR_age %>% filter(year >2025), 
                aes(ymin = IRR_q025, ymax = IRR_q975, col = model),lty = 2, alpha = 0, 
                show.legend = FALSE) +    
    facet_grid(facet_row~Transmission_assumption, switch = "y") + 
    theme_bw() + 
    coord_cartesian(ylim = c(1,35)) + 
    scale_y_log10(breaks = c(1, 2, 5, 10, 20, 30)) +
    scale_x_continuous(breaks = c(1990,  2005, 2015, 2025), limits = c(1990,2025)) + 
    scale_alpha_manual(values = alpha_values) + 
    scale_color_manual(values = colors_model) + 
    scale_fill_manual(values = colors_model) + 
    theme(legend.position =  c(0.02, 0.15), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 8), 
          strip.text = element_text(size = 9)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "horizontal", order = 1), 
           fill = guide_legend(direction = "horizontal", order = 1), 
           alpha = guide_legend(direction = "horizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(y= NULL, x = NULL, col = "Scenario", fill = "Scenario", alpha = "Scenario"))


####################################
## Plot Prevalence pregnant women ##
####################################

data_anc = data_models_30y %>% select(Year, model, FSW_assumption, Transmission_assumption,  
                                       starts_with("HIV_prev_ANC_adju")) %>% 
  rename_with(~ gsub("HIV_prev_ANC_adju", "Mean", .), ends_with("Mean")) %>% 
  rename_with(~ gsub("HIV_prev_ANC_adju", "LL", .), ends_with("LL")) %>% 
  rename_with(~ gsub("HIV_prev_ANC_adju", "UL", .), ends_with("UL")) %>% 
  pivot_longer(cols = - c(Year,model, FSW_assumption, Transmission_assumption),  
               names_to = c(".value", "age_group"), 
               names_sep = "_") %>% 
  mutate(age_group = case_when(age_group == "15" ~ "15-19", 
                               age_group == "20" ~ "20-24", 
                               age_group == "25" ~ "25-29", 
                               age_group == "30" ~ "30-34", 
                               age_group == "35" ~ "35-39")) %>% 
  mutate(model = gsub("model ", "", model)) %>% 
  mutate(FSW_assumption = case_when(model %in% c("1a", "2a", "3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
  mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
  mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption)) # or declining instead of change ? 

data_anc_1990_2023 = data_anc %>% 
  filter(Year >=1990, Year<=2023) %>% 
  mutate(age_group = paste0(age_group, " years")) 


(p_anc.prev = data_anc_1990_2023 %>% 
      ggplot(aes(x=Year, y=Mean)) + 
    geom_line(data = data_anc_1990_2023 %>% filter(FSW_assumption == "b - increasing FSW age & SW duration"), 
                                           aes(col = model, linetype = FSW_assumption), size = 0.8) + 
    geom_line(data = data_anc_1990_2023 %>% filter(FSW_assumption == "a - constant FSW age & SW duration"), 
                                         aes(col = model, linetype = FSW_assumption), size = 1.2) + 
    geom_ribbon(aes(ymin = LL, 
                    ymax = UL, fill = model), 
                alpha = 0.15) + theme_bw() + 
    scale_color_manual(values = c(colors_model), name="Scenario") + 
    scale_fill_manual(values = c(colors_model), name="Scenario") + 
    scale_y_continuous(labels = scales::percent) + 
    geom_pointrange(data = ANC_prev_data_tot %>% filter(age_group !="overall") %>% 
                      mutate(age_group = paste0(age_group, " years")),
                    aes(y = ANC_prev2, ymin = ANC_prev2-ANC_prev_lw, 
                            ymax = ANC_prev2 + ANC_prev_up, shape = "HIV prevalence ANC\nsurvey data"), size = 0.3) + 
    scale_shape_manual(values = 20) +
    labs(y= "HIV prevalence in pregnant women", x = NULL, shape = NULL) + 
    facet_grid(age_group ~ Transmission_assumption) + 
    theme(legend.position = c(0.02, 0.95), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(1.2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 9), 
          strip.text = element_text(size = 10)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "horizontal", order = 1), 
           fill = guide_legend(direction = "hoizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3))) 


##############################################
## Plot: HIV prevalence general populations ##
##############################################

data_prev_models = data_prev_models %>% 
  rename(age_group = age) %>% 
  mutate(sex = case_when(sex == "Males" ~ "Male", 
                         sex == "Females" ~ "Female")) %>% 
  mutate(year = as.integer(as.character(HSRCcalib)))


HIVprevData %>%  filter(year !=2016) %>% 
  mutate(sex = case_when(sex == "male" ~ "Male", 
                         sex == "female" ~ "Female")) %>%   
  ggplot(aes(x=age_group, y=prev)) + 
  geom_pointrange(aes(shape = "HIV prevalence\nsurvey data", 
                    ymin = prev - 1.96*SE, ymax = prev + 1.96*SE), 
                  size = 0.6) + 
  facet_grid(year ~ sex) + theme_bw() + 
  scale_y_continuous(labels = scales::percent) + 
  geom_pointrange(data = data_prev_models %>% filter(year !=2002) %>% 
                    mutate(model = gsub("model ", "", model)), 
                  aes(y = Mean, ymin = LL, ymax = UL, col = model), 
                  position = position_dodge(0.8), size = 0.1) + 
  scale_color_manual(values = c(colors_model), name="Scenario") + 
  scale_shape_manual(values = 20, name = NULL) + 
  labs(x="Age group", y = "HIV prevalence general population") + 
  theme(legend.position = "right", 
        legend.box.margin=margin(-2,-2,-2,-2), 
        legend.background = element_blank()) + 
  theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
        legend.text = element_text(size = 9), 
        legend.title = element_text(size =9), 
        axis.text = element_text(size = 9, angle = 45, hjust = 1), 
        strip.text = element_text(size = 10)) + 
  scale_linetype_manual(values = c(3,1), name = NULL) 


######################################################################
## Plot: Sensitivity analyses (seperate FSW age VERSUS SW duration) ##
######################################################################
  
(p_HIV_inFSW_sens= data_tot2_sens %>% 
   filter(Year <=2025) %>% 
   mutate(FSW_assumption = case_when( grepl("b\\[age\\]", FSW_assumption) ~ "b *age only* - increasing FSW age & constant SW duration",
                                      grepl("b\\[duration\\]", FSW_assumption) ~ "b *duration only* - constant FSW age & increasing SW duration", 
                                     grepl("a - constant", FSW_assumption) ~ "a - constant FSW age & SW duration", 
                                     grepl("b - changing", FSW_assumption) ~ "b - increasing FSW age & SW duration")
                                    ) %>% 
   mutate(model = gsub("model ", "", model)) %>% 
   mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                         "HIV prevalence in FSW", 
                                         "VL suppression in FSW"))) %>% 
   mutate(Transmission_assumption = gsub("probability", "risk in FSW", Transmission_assumption)) %>%
   mutate(Transmission_assumption = gsub("constant transmission risk", "constant\ntransmission risk", Transmission_assumption)) %>% 
   mutate(Transmission_assumption = gsub("declining transmission risk", "declining\ntransmission risk", Transmission_assumption)) %>% 
   mutate(Transmission_assumption = gsub("dynamically changing transmission risk", "exposure-dependent\ntransmission risk", Transmission_assumption)) %>% # or declining instead of change ? 
   ggplot(aes(x=Year, y=y)) + 
   theme_bw() + 
   geom_line(aes(col = FSW_assumption, linetype = FSW_assumption), size = 0.8) + 
   scale_color_manual(values = c(colors_fswassumption), name=NULL) + 
   labs(y=NULL, x = NULL) + 
   scale_y_continuous(labels = scales::percent,  limits = c(0,1)) + 
   facet_grid(type ~ Transmission_assumption, scales = "free_y", switch = "y") + 
   theme(legend.position = "top") +  
   geom_pointrange(data = FSW_prev_data ,
                   aes(y = prev, ymin = tot_prev_lwr, ymax=tot_prev_upr, group = dodge_group,
                       shape = "Calibration data"),col= "gray70", size = 0.1, position = position_dodge(1)) +
   geom_pointrange(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                      "HIV prevalence in FSW", 
                                                                                      "VL suppression in FSW"))),
                   aes(y = est, ymin = lb, ymax = ub, x = year,
                       shape = shape), size = 0.3) +
   scale_shape_manual(values = c(1,8), breaks= c("Validation data", "Calibration data"), 
                      labels = c("Validation data", 
                                 "Calibration data"),  name = NULL) +    
   theme(legend.position = "top", 
         legend.box = "horizontal",
         legend.justification="left",
         legend.margin=margin(-2,-2,-2,-2),
         legend.box.margin=margin(-2,-2,-2,-2), 
         legend.background = element_blank(), 
         legend.spacing.x = unit(1.2, "cm")) + 
   theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
         legend.text = element_text(size = 9), 
         legend.title = element_text(size =9), 
         axis.text = element_text(size = 9), 
         strip.text = element_text(size = 10)) + 
   scale_linetype_manual(values = c(3,1,2,2), name = NULL) + 
   guides(color = guide_legend(direction = "vertical", order = 1), 
          linetype = guide_legend(direction = "vertical", order = 1), 
          shape = guide_legend(direction = "vertical", order = 2))) 


#####################################################
### sensitivity analysis: open-cohort formulation ###
#####################################################


temp1 = data_tot2 %>% filter(model %in% c("model 3a", "model 3b")) %>% 
  mutate(color = gsub("model ", "", model)) %>% 
  mutate(color = paste0(color, " (Main analysis - closed cohort approximation)")) %>% 
  filter(Year <=2025) %>% 
  mutate(FSW_assumption = case_when(model %in% c("model 3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                        "HIV prevalence in FSW", 
                                        "VL suppression in FSW"))) %>% 
  mutate(model = gsub("model", "Scenario", model))

temp2 =  data_tot2_opencohort %>% 
  filter(analysis == "open cohort (kappa=15)") %>% 
  mutate(color = "Sensitivity analysis - open cohort") %>% 
  filter(Year <=2025) %>% 
  mutate(FSW_assumption = case_when(model %in% c("model 3a") ~ "a - constant FSW age & SW duration", 
                                    TRUE ~ "b - increasing FSW age & SW duration")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                        "HIV prevalence in FSW", 
                                        "VL suppression in FSW"))) %>% 
  mutate(model = gsub("model", "Scenario", model))


(p_HIV_inFSW_opencohort = temp1 %>% 
    ggplot(aes(x=Year, y=y)) + 
    theme_bw() + 
    geom_line(aes(col = color), size = 0.8) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = color), alpha = 0.3) +
    
    geom_line(data = temp2, aes(col = color), lty = 2, size = 0.8) + 
    geom_ribbon(data = temp2, aes(ymin = ymin, ymax = ymax, col = color, fill= color), alpha = 0.0, 
                lty = 3, size = 0.6) +
    
    scale_color_manual(values = c(colors_model[c(5,6)],"black"), name=NULL) + 
    scale_fill_manual(values = c(colors_model[c(5,6)], "white"), name=NULL) +     
    
    labs(y=NULL, x = NULL) + 
    scale_y_continuous(labels = scales::percent,  limits = c(0,1)) + 
    facet_grid(type ~ model, scales = "free_y", switch = "y") + 
    theme(legend.position = "top") +  
    geom_pointrange(data = data_reshandjaff4 %>% mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                                                                       "HIV prevalence in FSW", 
                                                                                       "VL suppression in FSW"))),
                    aes(y = est, ymin = lb, ymax = ub, x = year,
                        shape = shape), size = 0.3) +
    scale_shape_manual(values = c(1,8), breaks= c("Validation data", "Calibration data"), labels = c("Validation data", 
                                                                                                     "Calibration data"),  name = NULL) +    
    theme(legend.position = c(0.02, 0.95), 
          legend.box = "horizontal",
          legend.justification="left",
          legend.margin=margin(-2,-2,-2,-2),
          legend.box.margin=margin(-2,-2,-2,-2), 
          legend.background = element_blank(), 
          legend.spacing.x = unit(1.2, "cm")) + 
    theme(plot.caption = element_text(hjust=0, face = "bold", size = 9),
          legend.text = element_text(size = 9), 
          legend.title = element_text(size =9), 
          axis.text = element_text(size = 9), 
          strip.text = element_text(size = 10)) + 
    scale_linetype_manual(values = c(3,1), name = NULL) + 
    guides(color = guide_legend(direction = "vertical", order = 1), 
           fill = guide_legend(direction = "vertical", order = 1), 
           alpha = guide_legend(direction = "vertical", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)))


