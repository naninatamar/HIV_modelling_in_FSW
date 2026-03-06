## load packages
require(pals)
require(tidyverse)
require(ggh4x)
require(grid)

source("01_data_management.R")
## colors for the six models: 

colors_model = c(stepped2(2)[2], stepped(14)[14], stepped2(20)[15], stepped3(7)[7], stepped3(11)[9], stepped2(11)[c(7)])

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

ptemp = cowplot::plot_grid(pfswchar, blank, nrow = 1, rel_widths = c(0.7, 1))
cowplot::plot_grid(ptemp, ptrans, ncol = 1, labels = LETTERS, rel_heights = c(0.6, 0.4))

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
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = model), alpha = 0.3) +
    scale_color_manual(values = c(colors_model), name="Scenario") + 
    scale_fill_manual(values = c(colors_model), name="Scenario") + 
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
           fill = guide_legend(direction = "hoizontal", order = 1), 
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
                aes(ymin = PAF_q025, ymax = PAF_q975, fill = model), alpha = 0.3) +
    geom_ribbon(data = temp_data_correct %>% filter(year >2025), 
                aes(ymin = PAF_q025, ymax = PAF_q975, col = model),lty = 2, alpha = 0) +    
    facet_grid(facet_row~Transmission_assumption, switch = "y") + 
    coord_cartesian(ylim = c(0,0.5)) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() + 
    scale_color_manual(values = colors_model) + 
    scale_fill_manual(values = colors_model) + 
    labs(y=NULL, color = "Scenario", 
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
           fill = guide_legend(direction = "hoizontal", order = 1), 
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
                   aes(ymin = PAF_q025, ymax = PAF_q975, fill = model), alpha = 0.3) +
       geom_ribbon(data = distribution_paf %>% filter(year >2025), 
                   aes(ymin = PAF_q025, ymax = PAF_q975, col = model),lty = 2, alpha = 0) +    
       facet_grid(facet_row~Transmission_assumption, switch = "y", scales = "free") + 
       coord_cartesian(ylim = c(0,0.2)) +
       scale_y_continuous(labels = scales::percent) +
       theme_bw() + 
       scale_color_manual(values = colors_model) + 
       scale_fill_manual(values = colors_model) + 
       labs(y=NULL, color = "Scenario", 
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
              fill = guide_legend(direction = "hoizontal", order = 1), 
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
                aes(ymin = share_q025, ymax = share_q975, fill = model), alpha = 0.3) +
    geom_ribbon(data = relPAF %>% filter(year >2025), 
                aes(ymin = share_q025, ymax = share_q975, col = model),lty = 2, alpha = 0) +    
    facet_grid(.~Transmission_assumption, switch = "y") + 
    scale_y_continuous(labels = scales::percent) + 
    theme_minimal() + 
    scale_color_manual(values = colors_model) + 
    scale_fill_manual(values = colors_model) + 
    labs(y=NULL, color = "Scenario", 
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
           fill = guide_legend(direction = "hoizontal", order = 1), 
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
                aes(ymin = IRR_q025, ymax = IRR_q975, fill = model), alpha = 0.3) +
    geom_ribbon(data = IRR_age %>% filter(year >2025), 
                aes(ymin = IRR_q025, ymax = IRR_q975, col = model),lty = 2, alpha = 0) +    
    facet_grid(facet_row~Transmission_assumption, switch = "y") + 
    theme_bw() + 
    coord_cartesian(ylim = c(1,35)) + 
    scale_y_log10(breaks = c(1, 2, 5, 10, 20, 30)) +
    scale_x_continuous(breaks = c(1990,  2005, 2015, 2025), limits = c(1990,2025)) + 
    
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
           fill = guide_legend(direction = "hoizontal", order = 1), 
           linetype = guide_legend(order = 2), 
           shape = guide_legend(order = 3)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(y= NULL, x = NULL, col = "Scenario", fill = "Scenario"))


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

