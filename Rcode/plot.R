require(tidyverse)
require(reshape2)

# IR_r0p3_tofit: Data with intruder r = 30%
# IR_r0p1_tofit: Data with intruder r = 25%
# IR_r0p2_tofit: Data with intruder r = 20%

# notw_IR_r0p3_final: Synthesizer with intruder r = 30%
# notw_IR_r0p1_final: Synthesizer with intruder r = 25%
# notw_IR_r0p2_final: Synthesizer with intruder r = 20%

# w_r0p2_IR_r0p3_final: Marginal with agency r = 20% and intruder r = 30%
# w_r0p2_IR_r0p1_final: Marginal with agency r = 20% and intruder r = 25%
# w_r0p2_IR_r0p2_final: Marginal with agency r = 20% and intruder r = 20%

# w_r0p3_IR_r0p3_final: Marginal with agency r = 30% and intruder r = 30%
# w_r0p3_IR_r0p1_final: Marginal with agency r = 30% and intruder r = 25%
# w_r0p3_IR_r0p2_final: Marginal with agency r = 30% and intruder r = 20%

# w_r0p1_IR_r0p3_final: Marginal with agency r = 25% and intruder r = 30%
# w_r0p1_IR_r0p1_final: Marginal with agency r = 25% and intruder r = 25%
# w_r0p1_IR_r0p2_final: Marginal with agency r = 25% and intruder r = 20%



cbPalette <- c("#999999",  "#E69F00", "#56B4E9",  "#009E73", "#CC79A7", "#D55E00",  "#F0E442", "#0072B2")

IR <- as.data.frame(cbind(IR_r0p3_tofit,
                          IR_r0p25_tofit,
                          IR_r0p2_tofit,
                          IR_r0p15_tofit))
names(IR) <- c("r = 30%", "r = 25%", "r = 20%", "r = 15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  ggtitle("IR of real data under intruder r") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

Alphas <- as.data.frame(cbind(stan_data_w_r0p3$alphas,
                              stan_data_w_r0p25$alphas,
                              stan_data_w_r0p2$alphas,
                              stan_data_w_r0p15$alphas))
names(Alphas) <- c("r = 30%", "r = 25%", "r = 20%", "r = 15%")
Alphas_long = melt(Alphas)
ggplot(Alphas_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Alphas") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Synthesizers under different intruder r") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p3$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("Intruder r = 30%")



df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p25$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("Intruder r = 25%")

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p2$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("Intruder r = 20%")


df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p15$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("Intruder r = 15%")

IR <- as.data.frame(cbind(notw_IR_r0p3_final,
                          notw_IR_r0p25_final,
                          notw_IR_r0p2_final,
                          notw_IR_r0p15_final))
names(IR) <- c("r = 30%", "r = 25%", "r = 20%", "r = 15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  ggtitle("Synthesizers under different intruder r") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

#######################################################
############### one r = 0.3 ###########################
#######################################################

IR <- as.data.frame(cbind(IR_r0p3_tofit,
                          notw_IR_r0p3_final,
                          w_r0p3_IR_r0p3_final))
names(IR) <- c("Data", "Synthesizer", "Marginal")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  # ggtitle("r = 30%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#######################################################
############### one r = 0.25 ##########################
#######################################################

IR <- as.data.frame(cbind(IR_r0p25_tofit,
                          notw_IR_r0p25_final,
                          w_r0p25_IR_r0p25_final))
names(IR) <- c("Data", "Synthesizer", "Marginal")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  # ggtitle("r = 25%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#######################################################
############### one r = 0.2 ###########################
#######################################################

IR <- as.data.frame(cbind(IR_r0p2_tofit,
                          notw_IR_r0p2_final,
                          w_r0p2_IR_r0p2_final))
names(IR) <- c("Data", "Synthesizer", "Marginal")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  # ggtitle("r = 20%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

#######################################################
############### one r = 0.15 ##########################
#######################################################

IR <- as.data.frame(cbind(IR_r0p15_tofit,
                          notw_IR_r0p15_final,
                          w_r0p2_IR_r0p15_final))

names(IR) <- c("Data", "Synthesizer", "Marginal")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  # ggtitle("r = 20%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#######################################################
############### one r in one plot #####################
#######################################################
cbPalette_facet <- c("#999999",  "#E69F00", "#56B4E9",  "#999999",  "#E69F00", "#56B4E9",  
                     "#999999",  "#E69F00", "#56B4E9",  "#999999",  "#E69F00", "#56B4E9")


IR <- as.data.frame(c(IR_r0p3_tofit,
                          notw_IR_r0p3_final,
                          w_r0p3_IR_r0p3_final,
                          IR_r0p25_tofit,
                          notw_IR_r0p25_final,
                          w_r0p25_IR_r0p25_final,
                          IR_r0p2_tofit,
                          notw_IR_r0p2_final,
                          w_r0p2_IR_r0p2_final,
                          IR_r0p15_tofit,
                          notw_IR_r0p15_final,
                          w_r0p15_IR_r0p15_final))
names(IR) <- c("IR")
IR$Radius <- c(rep("r = 30%", 3000), rep("r = 25%", 3000),
               rep("r = 20%", 3000), rep("r = 15%", 3000))
IR$Radius <- factor(IR$Radius, levels = c("r = 30%", "r = 25%", "r = 20%", "r = 15%"))

IR$Model <- c(rep("D", 1000), rep("S", 1000), rep("M", 1000),
             rep("D", 1000), rep("S", 1000), rep("M", 1000),
             rep("D", 1000), rep("S", 1000), rep("M", 1000),
             rep("D", 1000), rep("S", 1000), rep("M", 1000))
IR$Model <- factor(IR$Model, levels = c("D", "S", "M"))

ggplot(IR, aes(x = Model, y = IR, color = Model)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette_facet) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  facet_wrap(vars(Radius), nrow = 2) + 
  #facet_grid(cols = vars(Radius)) +
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

#######################################################
############### intruder r = 30% ######################
#######################################################

IR <- as.data.frame(cbind(IR_r0p3_tofit, 
                          notw_IR_r0p3_final,
                          w_r0p3_IR_r0p3_final,
                          w_r0p25_IR_r0p3_final,
                          w_r0p2_IR_r0p3_final,
                          w_r0p15_IR_r0p3_final))
names(IR) <- c("Data", "Synthesizer", "30%", "25%", "20%", "15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  # ggtitle("Intruder r = 30%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))



#######################################################
############### intruder r = 25% ######################
#######################################################

IR <- as.data.frame(cbind(IR_r0p25_tofit, 
                          notw_IR_r0p25_final,
                          w_r0p3_IR_r0p25_final,
                          w_r0p25_IR_r0p25_final,
                          w_r0p2_IR_r0p25_final,
                          w_r0p15_IR_r0p25_final))
names(IR) <- c("Data", "Synthesizer", "30%", "25%", "20%", "15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 25%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

#######################################################
############### intruder r = 20% ######################
#######################################################

IR <- as.data.frame(cbind(IR_r0p2_tofit, 
                          notw_IR_r0p2_final,
                          w_r0p3_IR_r0p2_final,
                          w_r0p25_IR_r0p2_final,
                          w_r0p2_IR_r0p2_final,
                          w_r0p15_IR_r0p2_final))
names(IR) <- c("Data", "Synthesizer", "30%", "25%", "20%", "15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 20%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#######################################################
############### intruder r = 15% ######################
#######################################################

IR <- as.data.frame(cbind(IR_r0p15_tofit, 
                          notw_IR_r0p15_final,
                          w_r0p3_IR_r0p15_final,
                          w_r0p25_IR_r0p15_final,
                          w_r0p2_IR_r0p15_final,
                          w_r0p15_IR_r0p15_final))
names(IR) <- c("Data", "Synthesizer", "30%", "25%", "20%", "15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 15%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#######################################################
############### same r comparison #####################
#######################################################

cbPalette_nodatanosynthesizer <- c("#56B4E9",  "#009E73", "#CC79A7", "#D55E00",  "#F0E442", "#0072B2")


IR <- as.data.frame(cbind(w_r0p3_IR_r0p3_final,
                          w_r0p25_IR_r0p25_final,
                          w_r0p2_IR_r0p2_final,
                          w_r0p15_IR_r0p15_final))
names(IR) <- c("r = 30%", "r = 25%", "r = 20%", "r = 15%")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette_nodatanosynthesizer) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

######## WAM ##########

cbPalette <- c("#999999",  "#E69F00", "#56B4E9",  "#009E73", "#CC79A7", "#D55E00",  "#F0E442", "#0072B2")

IR = as.data.frame(cbind(seq(1:n), notw_IR_r0p15_final, w_r0p15_IR_r0p15_final))
names(IR) = c("index", "Synthesizer", "Marginal")

ggplot(IR) + 
  geom_point(aes(x = Synthesizer, y = Marginal, 
                 colour = Synthesizer + 0.2 <= Marginal,
                 size = Synthesizer + 0.2 <= Marginal)) +
  scale_colour_manual(values = cbPalette) +
  xlim(0, 0.7) + ylim(0, 0.7) +
  xlab("Synthesizer IR") + ylab("Marginal IR") + 
  theme_bw(base_size = 30, base_family = "") +
  theme(legend.position = "none") +
  geom_abline(intercept = 0, slope = 1, color="dodgerblue2", 
              linetype="solid", size=1.5)


############################################
###### Marginal vs Pairwise ################
############################################


############################################
###### r = 15% #############################
############################################

IR <- as.data.frame(cbind(IR_r0p15_tofit, 
                          notw_IR_r0p15_final,
                          w_r0p15_IR_r0p15_final,
                          w_r0p15_IR_r0p15_final_pw))
names(IR) <- c("Data", "Synthesizer", "Marginal", "Pairwise")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 15%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p15$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 15%, marginal")

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p15_pw$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 15%, pairwise")


############################################
###### r = 20% #############################
############################################

IR <- as.data.frame(cbind(IR_r0p2_tofit, 
                          notw_IR_r0p2_final,
                          w_r0p2_IR_r0p2_final,
                          w_r0p2_IR_r0p2_final_pw))
names(IR) <- c("Data", "Synthesizer", "Marginal", "Pairwise")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 20%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p2$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 20%, marginal")

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p2_pw$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 20%, pairwise")


############################################
###### r = 25% #############################
############################################

IR <- as.data.frame(cbind(IR_r0p25_tofit, 
                          notw_IR_r0p25_final,
                          w_r0p25_IR_r0p25_final,
                          w_r0p25_IR_r0p25_final_pw))
names(IR) <- c("Data", "Synthesizer", "Marginal", "Pairwise")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 25%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p25$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 25%, marginal")

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p25_pw$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 25%, pairwise")



############################################
###### r = 30% #############################
############################################

IR <- as.data.frame(cbind(IR_r0p3_tofit, 
                          notw_IR_r0p3_final,
                          w_r0p3_IR_r0p3_final,
                          w_r0p3_IR_r0p3_final_pw))
names(IR) <- c("Data", "Synthesizer", "Marginal", "Pairwise")
IR_long = melt(IR)
ggplot(IR_long, aes(x = variable, y = value, color = variable)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  #ggtitle("Intruder r = 30%") + 
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.3,
               position = position_dodge(width = .25))

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p3$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 30%, marginal")

df = as.data.frame(cbind(NBData$outcome, 
                         stan_data_w_r0p3_pw$alphas))
names(df) = c("Outcome", "Alpha")
ggplot(df, aes(x = Outcome, y = Alpha)) + geom_point(size = 3, position = "jitter") +
  ylim(0, 1) + # xlim(-10000, 2000000) + 
  theme_bw(base_size = 30, base_family = "") + 
  xlab("Outcome") + ylab("Alpha") + ggtitle("r = 30%, pairwise")


#######################################################
############### one r in one plot #####################
#######################################################
cbPalette_facet <- c("#999999",  "#E69F00", "#56B4E9",  "#009E73", "#999999",  "#E69F00", "#56B4E9", "#009E73",
                     "#999999",  "#E69F00", "#56B4E9",  "#009E73", "#999999",  "#E69F00", "#56B4E9", "#009E73")


IR <- as.data.frame(c(IR_r0p3_tofit,
                      notw_IR_r0p3_final,
                      w_r0p3_IR_r0p3_final,
                      w_r0p3_IR_r0p3_final_pw,
                      IR_r0p25_tofit,
                      notw_IR_r0p25_final,
                      w_r0p25_IR_r0p25_final,
                      w_r0p25_IR_r0p25_final_pw,
                      IR_r0p2_tofit,
                      notw_IR_r0p2_final,
                      w_r0p2_IR_r0p2_final,
                      w_r0p2_IR_r0p2_final_pw,
                      IR_r0p15_tofit,
                      notw_IR_r0p15_final,
                      w_r0p15_IR_r0p15_final,
                      w_r0p15_IR_r0p15_final_pw))
names(IR) <- c("IR")
IR$Radius <- c(rep("r = 30%", 4000), rep("r = 25%", 4000),
               rep("r = 20%", 4000), rep("r = 15%", 4000))
IR$Radius <- factor(IR$Radius, levels = c("r = 30%", "r = 25%", "r = 20%", "r = 15%"))

IR$Model <- c(rep("D", 1000), rep("S", 1000), rep("M", 1000), rep("P", 1000),
              rep("D", 1000), rep("S", 1000), rep("M", 1000), rep("P", 1000),
              rep("D", 1000), rep("S", 1000), rep("M", 1000), rep("P", 1000),
              rep("D", 1000), rep("S", 1000), rep("M", 1000), rep("P", 1000))
IR$Model <- factor(IR$Model, levels = c("D", "S", "M", "P"))

ggplot(IR, aes(x = Model, y = IR, color = Model)) +
  geom_violin(trim=TRUE) + theme_bw(base_size = 30, base_family = "") +
  scale_colour_manual(values = cbPalette_facet) +
  ylab("Identification Risks") + xlab("") + labs(color='') + ylim(0, 1) + 
  facet_wrap(vars(Radius), nrow = 2) + 
  #facet_grid(cols = vars(Radius)) +
  theme(legend.position="none")  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar",
               width = 0.25,
               position = position_dodge(width = .25))


#### WAM #####
cbPalette <- c("#999999",  "#E69F00", "#56B4E9",  "#009E73", "#CC79A7", "#D55E00",  "#F0E442", "#0072B2")

IR = as.data.frame(cbind(seq(1:n), notw_IR_r0p15_final, w_r0p15_IR_r0p15_final_pw))
names(IR) = c("index", "Synthesizer", "Pairwise")

ggplot(IR) + 
  geom_point(aes(x = Synthesizer, y = Pairwise, 
                 colour = Synthesizer + 0.2 <= Pairwise,
                 size = Synthesizer + 0.2 <= Pairwise)) +
  scale_colour_manual(values = cbPalette) +
  xlim(0, 0.7) + ylim(0, 0.7) +
  xlab("Synthesizer IR") + ylab("Pairwise IR") + 
  theme_bw(base_size = 30, base_family = "") +
  theme(legend.position = "none") +
  geom_abline(intercept = 0, slope = 1, color="dodgerblue2", 
              linetype="solid", size=1.5)

