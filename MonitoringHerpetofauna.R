## Camera Testing Analysis ##
## Brown, Hannon, Maerz 2023 ##
## Last updated March 23, 2023 ##

## Load Libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(tidyverse)
library(ggpubr)
library(reshape)
library(reshape2)
library(tidyr)
library(plyr)
library(ggExtra)
library(stringr)
library(gridExtra)
library(RColorBrewer)
library(lattice)
library(plot)

### Snakes ###
## Import Data
CameraTrap <- read.csv('CameraTrapData_Main.csv')
SnakeData <- read.csv('SnakeData.csv')
CamTrap2 <- left_join(CameraTrap, select(SnakeData, c(sp_code,mass_g)), by = "sp_code")
CamTrap3 <- subset(CamTrap2,trigger==",motion,heat,")

# Create variable of absolute value of temp differential
CamTrap3$abs_temp_diff = ifelse(CamTrap3$temp_diff <0,CamTrap3$temp_diff*-1,CamTrap3$temp_diff)

## Standardize Covariates
tempdiff_scaled <- (CamTrap3$abs_temp_diff-mean(CamTrap3$abs_temp_diff))/sd(CamTrap3$abs_temp_diff)
mass_scaled <- (CamTrap3$mass_g-mean(CamTrap3$mass_g))/sd(CamTrap3$mass_g)

## Raw Data: Detection by Camera
CameraTrap %>% group_by(camera) %>% summarise(group.prob = mean(detection)) %>%
  ggplot(., aes(x = reorder(camera, -group.prob), y = group.prob)) +
  geom_col(fill = "gray", color = "black") +
  scale_y_continuous("Probability of detection") + scale_x_discrete("Camera", labels = c("Dark Ops","Covert A", "Reconyx White","Covert B","Reconyx IR","Recon","Moultrie B","Moultrie A","Stealth A","Stealth B")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

## Raw Data: Detection by Species
CameraTrap %>% group_by(sp_code) %>% summarise(group.prob = mean(detection)) %>%
  ggplot(., aes(x = reorder(sp_code, -group.prob), y = group.prob)) +
  geom_col(fill = "grey70", color = "black") +
  scale_y_continuous("Probability of detection") + scale_x_discrete("Species", labels = c("L. getula","P. gutatus","P. alleghaniensis","D. punctatus","S. occipitomaculata")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))
prob <- CameraTrap %>% group_by(sp_code) %>% summarise(group.prob = mean(detection))

## Raw Data: Detection by Substrate
CameraTrap %>% group_by(substrate) %>% summarise(group.prob = mean(detection)) %>%
  ggplot(., aes(x = substrate, y = group.prob)) +
  geom_col(fill = "grey70", color = "black") +
  scale_y_continuous("Detection") + scale_x_discrete("Substrate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))


## Create one model with all important variables
model <- glm(detection ~ mass_scaled * tempdiff_scaled + camera + substrate, data = CamTrap3, family = binomial(link = "logit"))
summary(model)

# Temperature, Mass, and Detection
plot1 <- plot_model(model, title = "", type = "pred", terms = c("tempdiff_scaled [all]", "mass_scaled[-0.7929008,-0.7812986,-0.7515091,0.6721124,1.6535960]"))
plot1 +
  scale_y_continuous("Probability of detection", limits = c(0,1)) +
  scale_x_continuous("Absolute Temperature Difference (Celcius)", breaks = c(-1.274541,1.09209,3.45873), labels = c("0","2.2","4.4")) +
  scale_color_manual(values = c("dark gray","dark gray","dark gray","dark blue", "dark green"), guide = FALSE) +
  scale_fill_manual(values = c("light gray","light gray","light gray","light blue", "light green")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))

# Camera and Detection
plot2 <- plot_model(model, title = "", type = "pred", terms = c("camera"), sort.est = TRUE, show.values = TRUE)
plot2 +
  coord_flip()+
  scale_y_continuous("Probability of detection", limits = c(0,1)) +
  scale_x_discrete("Camera", limits = c("Covert A","Reconyx White","Moultrie B","Stealth B","Browning Recon","Reconyx IR","Moultrie A","Stealth A","Covert B","Browning Dark Ops")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))



### Frogs ###
## Import Data
frog.dat <- read.csv("Gopher_frog_camera_trap_trials_2022.csv")

# create variable of whether a photo was taken
frog.dat$photo <- ifelse(frog.dat$No.of.Pictures >0,1,0)

# create variable of temp differential
frog.dat$temp.diff = frog.dat$Animal.Temp.C - frog.dat$Substrate.Temp.C

# create variable of absolute value of temp differential
frog.dat$abs.temp.diff = ifelse(frog.dat$temp.diff <0,frog.dat$temp.diff*-1,frog.dat$temp.diff)

## Standardize Covariates
atd_scaled <- (frog.dat$abs.temp.diff-mean(frog.dat$abs.temp.diff))/sd(frog.dat$abs.temp.diff)
m_scaled <- (frog.dat$Mass.g-mean(frog.dat$Mass.g))/sd(frog.dat$Mass.g)


## Create one model with all important variables
f.model <- glm(photo ~ m_scaled + atd_scaled, data=frog.dat, family=binomial(link="logit"))
summary(f.model)


# Temperature and detection
plot2 <- plot_model(f.model, title = "", type = "pred", terms = c("atd_scaled [all]"))
plot2 +
  scale_y_continuous("Probability of detection", limits = c(0,1)) +
  scale_x_continuous("Absolute Temperature Difference (Celcius)", breaks = c(-1.646187,0.419727,2.065914), labels = c("0","2.4","4.7")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5))