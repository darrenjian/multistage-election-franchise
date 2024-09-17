# Project: Election Admin
# Author: Darren Jian
# Description: Creates plots for variable impact on turnout across all data sets
# Input: summary_vap.csv, summary_vep.csv, summary_rvp.csv
# Output: vap_plot, vep_plot, rvp_plot


library(FSA)
library(tidyverse)
library(ggplot2)
library(dplyr)
source('help_functions.R')


options(scipen=999)

plotvars <- c("Felon Voting"
              , "Same Day Registration" 
              , "Pct Voters Purged"
              , "Registration Website"
              , "Strict ID Law"
              , "Photo ID Required"
              , "All-Mail Voting"
              , "Early Voting"
              , "Absentee List Voting"
              , "High Poll Density"
              , "Provisional Balloting"
              , "HAVA Spending"
              , "Republican Majority"
              , "Democratic Majority"
              , "% Highschool Grad" 
              , "% Unemployed"
              , "Ln Median Income"
              , "Median Age"
              , "% Women"
              , "% Black"
              , "% Latino" 
              , "Felon Voting*% Black"
              , "High Density*Pct Black"
)

# obtain VAP column summary
coefs_t.vap <- as.data.frame((lapply(models_t[1], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vap$variable <- plotvars
coefs_t.vap$Source="Original (EAC)"
rownames(coefs_t.vap)=NULL

coefs_t.vap.leip <- as.data.frame((lapply(models_t.leip[1], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vap.leip$variable <- plotvars
coefs_t.vap.leip$Source="Leip"
rownames(coefs_t.vap.leip)=NULL

coefs_t.vap.adj <- as.data.frame((lapply(models_t.adj[1], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vap.adj$variable <- plotvars
coefs_t.vap.adj$Source="CPS-Adjusted Leip"
rownames(coefs_t.vap.adj)=NULL

df_t.vap <- rbind(coefs_t.vap,coefs_t.vap.leip,coefs_t.vap.adj)
colnames(df_t.vap) <- c("estimate","se","variable","Source")
df_t.vap


# obtain VEP column summary
coefs_t.vep <- as.data.frame((lapply(models_t[2], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vep$variable <- plotvars
coefs_t.vep$Source="Original (EAC)"
rownames(coefs_t.vep)=NULL

coefs_t.vep.leip <- as.data.frame((lapply(models_t.leip[2], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vep.leip$variable <- plotvars
coefs_t.vep.leip$Source="Leip"
rownames(coefs_t.vep.leip)=NULL

coefs_t.vep.adj <- as.data.frame((lapply(models_t.adj[2], summary))[[1]]$coefficients[-1,1:2])
coefs_t.vep.adj$variable <- plotvars
coefs_t.vep.adj$Source="CPS-Adjusted Leip"
rownames(coefs_t.vep.adj)=NULL

df_t.vep <- rbind(coefs_t.vep,coefs_t.vep.leip,coefs_t.vep.adj)
colnames(df_t.vep) <- c("estimate","se","variable","Source")
df_t.vep


# obtain RVP column summary
coefs_t.rvp <- as.data.frame((lapply(models_t[3], summary))[[1]]$coefficients[-1,1:2])
coefs_t.rvp$variable <- plotvars
coefs_t.rvp$Source="Original (EAC)"
rownames(coefs_t.rvp)=NULL

coefs_t.rvp.leip <- as.data.frame((lapply(models_t.leip[3], summary))[[1]]$coefficients[-1,1:2])
coefs_t.rvp.leip$variable <- plotvars
coefs_t.rvp.leip$Source="Leip"
rownames(coefs_t.rvp.leip)=NULL

coefs_t.rvp.adj <- as.data.frame((lapply(models_t.adj[3], summary))[[1]]$coefficients[-1,1:2])
coefs_t.rvp.adj$variable <- plotvars
coefs_t.rvp.adj$Source="CPS-Adjusted Leip"
rownames(coefs_t.rvp.adj)=NULL

df_t.rvp <- rbind(coefs_t.rvp,coefs_t.rvp.leip,coefs_t.rvp.adj)
colnames(df_t.rvp) <- c("estimate","se","variable","Source")
df_t.rvp

pd = position_dodge(0.5)    # how much to jitter the points on the plot

# plot VAP variables
vap_plot <- df_t.vap %>%
  mutate(variable = fct_relevel(variable, 
                                rev(plotvars))) %>%
  ggplot(
    aes(x     = variable,
        y     = estimate,
        shape = Source,
        color = Source)
  ) +
  
  geom_point(size     = 1.5,
             position = pd) +
  
  geom_errorbar(aes(ymin  = estimate - 1.96 * se,
                    ymax  = estimate + 1.96 * se),
                width = 0.2,
                size  = 0.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position = "bottom") +
  ylab("Coefficient") +
  xlab("Variable") +
  ggtitle("Effect Size by Dataset (VAP)") +
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  ylim(-0.15,0.15)
vap_plot
ggplot_tex(vap_plot, 'out/', 6, 8.5)

# plot VEP variables
vep_plot <- df_t.vep %>%
  mutate(variable = fct_relevel(variable, 
                                rev(plotvars))) %>%
  ggplot(
    aes(x     = variable,
        y     = estimate,
        shape = Source,
        color = Source)
  ) +
  
  geom_point(size     = 1.5,
             position = pd) +
  
  geom_errorbar(aes(ymin  = estimate - 1.96 * se,
                    ymax  = estimate + 1.96 * se),
                width = 0.2,
                size  = 0.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position = "bottom") +
  ylab("Coefficient") +
  xlab("Variable") +
  ggtitle("Effect Size by Dataset (VEP)") +
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  ylim(-0.15,0.15)
vep_plot
ggplot_tex(vep_plot, 'out/', 6, 8.5)


# plot RVP variables
rvp_plot <- df_t.rvp %>%
  mutate(variable = fct_relevel(variable, 
                                rev(plotvars))) %>%
  ggplot(
    aes(x     = variable,
        y     = estimate,
        shape = Source,
        color = Source)
  ) +
  
  geom_point(size     = 2,
             position = pd) +
  
  geom_errorbar(aes(ymin  = estimate - 1.96 * se,
                    ymax  = estimate + 1.96 * se),
                width = 0.2,
                size  = 0.3,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), legend.position = "bottom") +
  ylab("Coefficient") +
  xlab("Variable") +
  ggtitle("Effect Size by Dataset (RVP)") +
  coord_flip() + 
  geom_hline(yintercept = 0) + 
  ylim(-0.15,0.15)
rvp_plot
ggplot_tex(rvp_plot, 'out/', 6, 8.5)

