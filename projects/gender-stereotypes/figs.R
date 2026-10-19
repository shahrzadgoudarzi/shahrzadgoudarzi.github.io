library(tidyverse)
library(haven)
library(ggplot2)
library(patchwork)


df <- read_dta("data1.dta")
View(df)

x_labels <- c("-1" = "-1\nWomen", "0" = "0\nEqual", "1" = "1\nMen")

#jitter raw points to avoid overplotting
df$communal_jitter <- pmin(pmax(df$communal + runif(nrow(df), -0.4, 0.4), -1), 1)
df$agentic_jitter <- pmin(pmax(df$agentic + runif(nrow(df), -0.4, 0.4), -1), 1)

# ====Fig. 1====
## ====Fig. 1A====

df1 <- read_dta("s1_margins_ms_com.dta")%>%
  mutate(communal = c(-1, 0, 1)) 

ms_com <- ggplot(df1, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = oldf_atw), 
              alpha = 0.2, size = .8, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Men's Supremacy",
    title = ""
  ) +
  theme_minimal()+
  theme(
    axis.title.y = element_text(size = 9)  
  )
ms_com

ggsave(
  filename = "Final Tables + Graphs/Figure1_A.pdf",
  plot     = ms_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 1B====
df2 <- read_dta("s1_margins_ds_com.dta")%>%
  mutate(communal = c(-1, 0, 1)) 

ds_com <- ggplot(df2, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = mod_sexism), 
              alpha = 0.2, size = .8, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Denial of Sexism",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
ds_com

ggsave(
  filename = "Final Tables + Graphs/Figure1_B.pdf",
  plot     = ds_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 1C====
df3 <- read_dta("s1_margins_rs_com.dta")%>%
  mutate(communal = c(-1, 0, 1))  

rs_com <- ggplot(df3, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = reverse_sexism), 
              alpha = 0.2, size = .8, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Reverse Sexism",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )

rs_com

ggsave(
  filename = "Final Tables + Graphs/Figure1_C.pdf",
  plot     = rs_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 1D====
df4 <- read_dta("s1_margins_tgr_com.dta")%>%
  mutate(communal = c(-1, 0, 1)) 

tgr_com <-ggplot(df4, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = eg_voc), 
              alpha = 0.2, size = .8, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Traditional Domestic Roles",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
tgr_com

ggsave(
  filename = "Figure1_D.pdf",
  plot     = tgr_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 1E====
df5 <- read_dta("s1_margins_sj_com.dta")%>%
  mutate(communal = c(-1, 0, 1))  

sj_com <- ggplot(df5, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = sj_m), 
              alpha = 0.2, size = .8, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "System Justification",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
sj_com

ggsave(
  filename = "Figure1_E.pdf",
  plot     = sj_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

# Combine
combined_plot <- (
  (ms_com  | ds_com ) /
    (rs_com  | tgr_com) /
    (  sj_com | plot_spacer())
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center"
  )
combined_plot <- combined_plot + 
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 12) 
  )
print(combined_plot)

tiff("S1_Fig1.tiff", width = 8, height = 9, units = "in", res = 600)
print(combined_plot)
dev.off()
# ====Fig. 2====
## ====Fig. 2A====
df6 <- read_dta("s1_margins_ms_agent.dta")%>%
  mutate(agentic = c(-1, 0, 1)) 

ms_agent <- ggplot(df6, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = oldf_atw), 
              alpha = 0.2, size = .8, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Men's Supremacy",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
ms_agent

ggsave(
  filename = "Figure2_A.pdf",
  plot     = ms_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 2B====
df7 <- read_dta("s1_margins_ds_agent.dta")%>%
  mutate(agentic = c(-1, 0, 1))  

ds_agent <- ggplot(df7, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = mod_sexism), 
              alpha = 0.2, size = .8, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Denial of Sexism",
    title = ""
  ) +
  theme_minimal()+ theme(
    axis.title.y = element_text(size = 9)  
  )
ds_agent

ggsave(
  filename = "Figure2_B.pdf",
  plot     = ds_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 2C====
df8 <- read_dta("s1_margins_rs_agent.dta")%>%
  mutate(agentic = c(-1, 0, 1)) 

rs_agent <- ggplot(df8, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = reverse_sexism), 
              alpha = 0.2, size = .8, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Reverse Sexism",
    title = ""
  ) +
  theme_minimal()+ theme(
    axis.title.y = element_text(size = 9)  
  )
rs_agent

ggsave(
  filename = "Figure2_C.pdf",
  plot     = rs_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 2D====

df9 <- read_dta("s1_margins_tgr_agent.dta")%>%
  mutate(agentic = c(-1, 0, 1))  

tgr_agent <-ggplot(df9, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = eg_voc), 
              alpha = 0.2, size = .8, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "Traditional Domestic Roles",
    title = ""
  ) +
  theme_minimal()+ theme(
    axis.title.y = element_text(size = 9)  
  )
tgr_agent

ggsave(
  filename = "Figure2_D.pdf",
  plot     = tgr_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 2E====
df10 <- read_dta("s1_margins_sj_agent.dta")%>%
  mutate(agentic = c(-1, 0, 1))  

sj_agent <- ggplot(df10, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = sj_m), 
              alpha = 0.2, size = .8, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(-1, 0, 1), labels = x_labels) +
  labs(
    x = NULL,
    y = "System Justification",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
sj_agent

ggsave(
  filename = "Figure2_E.pdf",
  plot     = sj_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)


combined_plot <- (
  (ms_agent  | ds_agent ) /
    (rs_agent  | tgr_agent) /
    (  sj_agent | plot_spacer())
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center"
  )
combined_plot <- combined_plot + 
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 12) 
  )
print(combined_plot)

tiff("S1_Fig2.tiff", width = 8, height = 9, units = "in", res = 600)
print(combined_plot)
dev.off()

# ====Fig. 3====
rm(list = ls())

df <- read_dta("data2.dta")
View(df)

x_labels <- c("1" = "1\nWomen", "4" = "4\nEqual", "7" = "7\nMen")

#jitter raw points to avoid overplotting
df$communal_jitter <- pmin(pmax(df$communal + runif(nrow(df), -0.4, 0.4), 1), 7)
df$agentic_jitter <- pmin(pmax(df$agentic + runif(nrow(df), -0.4, 0.4), 1), 7)

## ====Fig. 3A====

df1 <- read_dta("s2_margins_hs_com.dta")%>%
  mutate(communal = c(1, 4, 7)) 

hs_com <- ggplot(df1, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = hsi), 
              alpha = 0.1, size = .5, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = NULL,
    y = "Hostile Sexism",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
hs_com

ggsave(
  filename = "Figure3_A.pdf",
  plot     = hs_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 3B====

df2 <- read_dta("s2_margins_bs_com.dta")%>%
  mutate(communal = c(1, 4, 7)) 

bs_com <- ggplot(df2, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = bsi), 
              alpha = 0.1, size = .5, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = NULL,
    y = "Benevolent Sexism",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
bs_com

ggsave(
  filename = "Figure3_B.pdf",
  plot     = bs_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 3C====

df3 <- read_dta("s2_margins_ds_com.dta")%>%
  mutate(communal = c(1, 4, 7)) 

ds2_com <- ggplot(df3, aes(x = communal, y = `_margin`)) +
  geom_jitter(data = df, aes(x = communal_jitter, y = revawareness_gender_inequality), 
              alpha = 0.1, size = .5, color = "steelblue") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = "Communal",
    y = "Denial of Sexism",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
ds2_com

ggsave(
  filename = "Figure3_C.pdf",
  plot     = ds2_com,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)
## ====Fig. 3D====
df4 <- read_dta("s2_margins_hs_agent.dta")%>%
  mutate(agentic = c(1, 4, 7)) 

hs_agent <- ggplot(df4, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = hsi), 
              alpha = 0.1, size = .5, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = NULL,
    y = "",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
hs_agent

ggsave(
  filename = "Figure3_D.pdf",
  plot     = hs_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)

## ====Fig. 3E====
df5 <- read_dta("s2_margins_bs_agent.dta")%>%
  mutate(agentic = c(1, 4, 7)) 
bs_agent <- ggplot(df5, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = bsi), 
              alpha = 0.1, size = .5, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = NULL,
    y = "",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
bs_agent

ggsave(
  filename = "Final Tables + Graphs/Figure3_E.pdf",
  plot     = bs_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)


## ====Fig. 3F====
df6 <- read_dta("s2_margins_ds_agent.dta")%>%
  mutate(agentic = c(1, 4, 7)) 

ds2_agent <- ggplot(df6, aes(x = agentic, y = `_margin`)) +
  geom_jitter(data = df, aes(x = agentic_jitter, y = revawareness_gender_inequality), 
              alpha = 0.1, size = .5, color = "firebrick") +
  geom_ribbon(aes(ymin = `_ci_lb`, ymax = `_ci_ub`), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black",linewidth = 1.1) +
  scale_x_continuous(breaks = c(1, 4, 7), labels = x_labels) +
  labs(
    x = "Agentic",
    y = "",
    title = ""
  ) +
  theme_minimal() + theme(
    axis.title.y = element_text(size = 9)  
  )
ds2_agent

ggsave(
  filename = "Figure3_F.pdf",
  plot     = ds2_agent,
  device   = "pdf",     
  width    = 4.5,
  height   = 3.5,
  units    = "in"
)


combined_plot <- (
  (hs_com  | hs_agent ) /
    (bs_com  | bs_agent) /
    (  ds2_com | ds2_agent)
) 

combined_plot <- combined_plot + 
  plot_annotation(tag_levels = list(c("A", "D", "B", "E", "C", "F"))) &
                    theme(
                      plot.tag = element_text(size = 12) 
                    )


print(combined_plot)

tiff("S2_Fig3.tiff", width = 8, height = 9, units = "in", res = 600)
print(combined_plot)
dev.off()
# ====Fig. 4====
# ====Interactions Plots Study 2====

cols <- c("-3" = "steelblue", "0" = "black", "3" = "firebrick")
labels <- c("-3" = "Women", "0" = "Equal", "3" = "Men")

## ====Fig. 4A====

df7 <- read_dta("s2_margins_inter_hsi.dta")
df7 <- df7 %>%
  rename(fit = `_margin`,
         x = `_at1`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at2`)

p_hsi <- ggplot(df7, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(0,5), breaks = 0:5) +
  scale_x_continuous(breaks = c(-3,0,3), labels = c("-3\nWomen","0\nEqual","3\nMen")) +
  labs(x = "", y = "HS") +
  theme_minimal() +
  theme(legend.position = "none")
p_hsi

## ====Fig. 4B====

df8 <- read_dta("s2_margins_inter_bsi.dta")
df8 <- df8 %>%
  rename(fit = `_margin`,
         x = `_at1`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at2`)

p_bsi <- ggplot(df8, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(0,5), breaks = 0:5) +
  scale_x_continuous(breaks = c(-3,0,3), labels = c("-3\nWomen","0\nEqual","3\nMen")) +
  labs(x = "", y = "BS") +
  theme_minimal() +
  theme(legend.position = "none")
p_bsi

## ====Fig. 4C====

df9 <- read_dta("s2_margins_inter_ds.dta")
df9 <- df9 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)

p_ds <- ggplot(df9, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-3,0,3), labels = c("-3\nWomen","0\nEqual","3\nMen")) +
  labs(x = "Agentic", y = "DS") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_ds



# Combine
combined_plot <- (p_hsi /p_bsi /  p_ds) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12, face = "plain"),
    plot.tag.position = c(-0.01, 1)
  )

print(combined_plot)

tiff("S2_interaction.tiff",
     width = 7, height = 12, units = "in",
     res = 600)
print(combined_plot)
dev.off()

# ====SM====
### ====Fig. S4====

## ====Interactions Plots Study 1====
# Common color mapping
cols <- c("-1" = "steelblue", "0" = "black", "1" = "firebrick")
labels <- c("-1" = "Women", "0" = "Equal", "1" = "Men")


### ====Fig. S4A====
# men's supremacy plot
df3 <- read_dta("Final Analysis/s1_margins_inter_ms.dta")
df3 <- df3 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)
p_ms <- ggplot(df3, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c("-1\nWomen","0\nEqual","1\nMen")) +
  labs(x = "", y = "Men's Supremacy") +
  theme_minimal() +
  theme(legend.position = "none")
p_ms

### ====Fig. S4B====
# denial of sexism plot
df4 <- read_dta("Final Analysis/s1_margins_inter_ds.dta")
df4 <- df4 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)

p_ds <- ggplot(df4, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c("-1\nWomen","0\nEqual","1\nMen")) +
  labs(x = "", y = "Denial of Sexism") +
  theme_minimal() +
  theme(legend.position = "none")
p_ds

### ====Fig. S4C====
# reverse sexism plot
df5 <- read_dta("s1_margins_inter_rs.dta")
df5 <- df5 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)

p_rs <- ggplot(df5, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c("-1\nWomen","0\nEqual","1\nMen")) +
  labs(x = "", y = "Reverse Sexism") +
  theme_minimal() +
  theme(legend.position = "none")
p_rs

### ====Fig. S4D====
# trad gender roles
df6 <- read_dta("s1_margins_inter_tgr.dta")
df6 <- df6 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)

p_tgr <- ggplot(df6, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c("-1\nWomen","0\nEqual","1\nMen")) +
  labs(x = "Agentic", y = "Traditional Domestic Roles") +
  theme_minimal() +
  theme(legend.position = "none")
p_tgr

### ====Fig. S4E====
# sj
df7 <- read_dta("s1_margins_inter_sj.dta")
df7 <- df7 %>%
  rename(fit = `_margin`,
         x = `_at2`,  
         lwr = `_ci_lb`,      
         upr = `_ci_ub`,
         group =`_at1`)

p_sj <- ggplot(df7, aes(x = x, y = fit, color = factor(group), group = factor(group))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(group)), alpha = 0.2, color = NA) +
  scale_color_manual(values = cols, labels = labels, name = "Communal") +
  scale_fill_manual(values = cols, labels = labels, name = "Communal") +
  scale_y_continuous(limits = c(1,7), breaks = 1:7) +
  scale_x_continuous(breaks = c(-1,0,1), labels = c("-1\nWomen","0\nEqual","1\nMen")) +
  labs(x = "Agentic", y = "System Justification") +
  theme_minimal() +
  theme(legend.position = "none")
p_sj


# Combine
combined_plot <- (
  (p_ms  | p_ds ) /
    (p_rs  | p_tgr ) /
    (  p_sj | plot_spacer())
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center"
  )
combined_plot <-combined_plot + plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 12, face = "plain"),
    plot.tag.position = c(-0.01, 1)
  )
print(combined_plot)
tiff("S1_interaction.tiff", width = 8, height = 9, units = "in", res = 600)
print(combined_plot)
dev.off()
