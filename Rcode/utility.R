
source("boot_stat_new.R")
source("boot_quantile_new.R")

cbPalette_withdata <- c("#0072B2", "#999999",  "#E69F00", "#56B4E9",  "#009E73", "#CC79A7", "#D55E00",  "#F0E442")

orig_data <- NBData
notw_data_1 <- data.frame(sim_syn_notw[[2]])
r0p25_data_1 <- data.frame(sim_syn_w_r0p25[[2]])
r0p3_data_1 <- data.frame(sim_syn_w_r0p3[[2]])
r0p2_data_1 <- data.frame(sim_syn_w_r0p2[[2]])
r0p15_data_1 <- data.frame(sim_syn_w_r0p15[[2]])
df <- data.frame(orig_data, notw_data_1, r0p3_data_1, r0p25_data_1, r0p2_data_1, r0p15_data_1)
names(df) <- c("Data", "Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df_long <- reshape2::melt(df)
ggplot(df_long, aes(x = variable, y = value, fill = variable, color = variable)) +
  geom_violin(trim=TRUE, alpha = 0.3) + 
  geom_boxplot(width=0.1) + 
  stat_summary(fun=mean, geom="point", size=3, color = "gray") +
  scale_colour_manual(values = cbPalette_withdata) + scale_fill_manual(values = cbPalette_withdata) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  ylab("outcome") + xlab("") #+ ylim(0, 1)


df <- data.frame(y = NBData$outcome)
ggplot(df, aes(y)) +
  geom_density() + 
  scale_colour_manual(values = cbPalette_withdata) + scale_fill_manual(values = cbPalette_withdata) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  ylab("density") + xlab("outcome") #+ ylim(0, 1)

m <- 20
S <- 1000
n <- N

# NBData: Data
# sim_syn_notw: Synthesizer
# sim_syn_w_r0p15: agency r = 0.05
# sim_syn_w_r0p1: agency r = 0.1
# sim_syn_w_r0p2: agency r = 0.2

###########################################
################# Data  ###################
###########################################
thedata       = vector("list",1)
thedata[[1]]  = NBData

mean_output_data = boot_stat(thedata, 1, mean, 123, S, n)
median_output_data = boot_stat(thedata, 1, median, 123, S, n)

mean_output_data
median_output_data


###########################################
################# synthesizer  ############
###########################################

mean_output_synthesizer = boot_stat(sim_syn_notw, m, mean, 123, S, n)
median_output_synthesizer = boot_stat(sim_syn_notw, m, median, 123, S, n)

mean_output_synthesizer
median_output_synthesizer




###########################################
############# agency r = 0.3  ############
###########################################

mean_output_r0p3 = boot_stat(sim_syn_w_r0p3, m, mean, 123, S, n)
median_output_r0p3 = boot_stat(sim_syn_w_r0p3, m, median, 123, S, n)

mean_output_r0p3
median_output_r0p3


###########################################
############# agency r = 0.25  #############
###########################################

mean_output_r0p25 = boot_stat(sim_syn_w_r0p25, m, mean, 123, S, n)
median_output_r0p25 = boot_stat(sim_syn_w_r0p25, m, median, 123, S, n)

mean_output_r0p25
median_output_r0p25

###########################################
############# agency r = 0.2  #############
###########################################

mean_output_r0p2 = boot_stat(sim_syn_w_r0p2, m, mean, 123, S, n)
median_output_r0p2 = boot_stat(sim_syn_w_r0p2, m, median, 123, S, n)

mean_output_r0p2
median_output_r0p2



###########################################
############# agency r = 0.15  ############
###########################################

mean_output_r0p15 = boot_stat(sim_syn_w_r0p15, m, mean, 123, S, n)
median_output_r0p15 = boot_stat(sim_syn_w_r0p15, m, median, 123, S, n)

mean_output_r0p15
median_output_r0p15

###########################################
############# plotting  ###################
###########################################

cbPalette_nodatanosyn <- c("#E69F00", "#56B4E9",  "#009E73", "#CC79A7", "#D55E00")

## mean

df <- as.data.frame(c(mean_output_synthesizer$PE,
                      mean_output_r0p3$PE,
                      mean_output_r0p25$PE,
                      mean_output_r0p2$PE,
                      mean_output_r0p15$PE))
names(df) <- c("estimate")
df$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df$Model <- factor(df$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))

df_lower <- as.data.frame(c(mean_output_synthesizer$lower,
                            mean_output_r0p3$lower,
                            mean_output_r0p25$lower,
                            mean_output_r0p2$lower,
                            mean_output_r0p15$lower))
names(df_lower) <- c("lower")
df_lower$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df_lower$Model <- factor(df_lower$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))


df_upper <- as.data.frame(c(mean_output_synthesizer$upper,
                            mean_output_r0p3$upper,
                            mean_output_r0p25$upper,
                            mean_output_r0p2$upper,
                            mean_output_r0p15$upper))
names(df_upper) <- c("upper")
df_upper$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df_upper$Model <- factor(df_upper$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))

ggplot(df, aes(x = Model, y = estimate, color = Model)) + 
  geom_point(size=8, shape=4) +
  scale_colour_manual(values = cbPalette_nodatanosyn) +
  geom_errorbar(aes(ymin = df_lower$lower, ymax = df_upper$upper), 
                colour=cbPalette_nodatanosyn, width=.2, size = 1.5, position = position_dodge(0.01)) +
  theme_bw(base_size = 25) +
  theme(legend.position = "none") +
  ylab("Mean Statistic") + xlab("") + ylim(92, 108) +
  geom_hline(yintercept = mean_output_data$PE, linetype="solid", 
             color = "#999999", size=1) +
  geom_hline(yintercept = mean_output_data$lower, linetype="dashed", 
             color = "#999999", size=1) +
  geom_hline(yintercept = mean_output_data$upper, linetype="dashed", 
             color = "#999999", size=1)

## median

df <- as.data.frame(c(median_output_synthesizer$PE,
                      median_output_r0p3$PE,
                      median_output_r0p25$PE,
                      median_output_r0p2$PE,
                      median_output_r0p15$PE))
names(df) <- c("estimate")
df$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df$Model <- factor(df$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))

df_lower <- as.data.frame(c(median_output_synthesizer$lower,
                            median_output_r0p3$lower,
                            median_output_r0p25$lower,
                            median_output_r0p2$lower,
                            median_output_r0p15$lower))
names(df_lower) <- c("lower")
df_lower$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df_lower$Model <- factor(df_lower$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))


df_upper <- as.data.frame(c(median_output_synthesizer$upper,
                            median_output_r0p3$upper,
                            median_output_r0p25$upper,
                            median_output_r0p2$upper,
                            median_output_r0p15$upper))
names(df_upper) <- c("upper")
df_upper$Model <- c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%")
df_upper$Model <- factor(df_upper$Model, levels = c("Synthesizer", "r = 30%", "r = 25%", "r = 20%", "r = 15%"))

ggplot(df, aes(x = Model, y = estimate, color = Model)) + 
  geom_point(size=8, shape=4) +
  scale_colour_manual(values = cbPalette_nodatanosyn) +
  geom_errorbar(aes(ymin = df_lower$lower, ymax = df_upper$upper), 
                colour=cbPalette_nodatanosyn, width=.2, size = 1.5, position = position_dodge(0.01)) +
  theme_bw(base_size = 25) +
  theme(legend.position = "none") +
  ylab("Median Statistic") + xlab("") + ylim(92, 108) +
  geom_hline(yintercept = median_output_data$PE, linetype="solid", 
             color = "#999999", size=1) +
  geom_hline(yintercept = median_output_data$lower, linetype="dashed", 
             color = "#999999", size=1) +
  geom_hline(yintercept = median_output_data$upper, linetype="dashed", 
             color = "#999999", size=1)

#####################################################
##### ECDF utility metric ###########################
#####################################################

source('calculate_ECDF.R')

ECDF_synthesizer <- calculate_ECDF(orig_data, syndata = sim_syn_notw, m = 20)
ECDF_synthesizer

ECDF_r0p3 <- calculate_ECDF(orig_data, syndata = sim_syn_w_r0p3, m = 20)
ECDF_r0p3

ECDF_r0p25 <- calculate_ECDF(orig_data, syndata = sim_syn_w_r0p25, m = 20)
ECDF_r0p25

ECDF_r0p2 <- calculate_ECDF(orig_data, syndata = sim_syn_w_r0p2, m = 20)
ECDF_r0p2

ECDF_r0p15 <- calculate_ECDF(orig_data, syndata = sim_syn_w_r0p15, m = 20)
ECDF_r0p15


###########################################
############# agency r = 0.15 pairwise  ###
###########################################

mean_output_r0p15_pw = boot_stat(sim_syn_w_r0p15_pw, m, mean, 123, S, n)
median_output_r0p15_pw = boot_stat(sim_syn_w_r0p15_pw, m, median, 123, S, n)

mean_output_r0p15_pw
median_output_r0p15_pw


###########################################
############# plotting  ###################
###########################################

cbPalette_nodatanosyn <- c("#E69F00", "#56B4E9",  "#009E73")

## mean

df <- as.data.frame(c(mean_output_synthesizer$PE,
                      mean_output_r0p15$PE,
                      mean_output_r0p15_pw$PE))
names(df) <- c("estimate")
df$Model <- c("Synthesizer", "Marginal", "Pairwise")
df$Model <- factor(df$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))

df_lower <- as.data.frame(c(mean_output_synthesizer$lower,
                            mean_output_r0p15$lower,
                            mean_output_r0p15_pw$lower))
names(df_lower) <- c("lower")
df_lower$Model <- c("Synthesizer", "Marginal", "Pairwise")
df_lower$Model <- factor(df_lower$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))


df_upper <- as.data.frame(c(mean_output_synthesizer$upper,
                            mean_output_r0p15$upper,
                            mean_output_r0p15_pw$upper))
names(df_upper) <- c("upper")
df_upper$Model <- c("Synthesizer", "Marginal", "Pairwise")
df_upper$Model <- factor(df_upper$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))

ggplot(df, aes(x = Model, y = estimate, color = Model)) + 
  geom_point(size=8, shape=4) +
  scale_colour_manual(values = cbPalette_nodatanosyn) +
  geom_errorbar(aes(ymin = df_lower$lower, ymax = df_upper$upper), 
                colour=cbPalette_nodatanosyn, width=.2, size = 1.5, position = position_dodge(0.01)) +
  theme_bw(base_size = 25) +
  theme(legend.position = "none") +
  ylab("Mean Statistic") + xlab("") + ylim(92, 108) +
  geom_hline(yintercept = mean_output_data$PE, linetype="solid", 
             color = "#999999", size=1) +
  geom_hline(yintercept = mean_output_data$lower, linetype="dashed", 
             color = "#999999", size=1) +
  geom_hline(yintercept = mean_output_data$upper, linetype="dashed", 
             color = "#999999", size=1)

## median

df <- as.data.frame(c(median_output_synthesizer$PE,
                      median_output_r0p15$PE,
                      median_output_r0p15_pw$PE))
names(df) <- c("estimate")
df$Model <- c("Synthesizer", "Marginal", "Pairwise")
df$Model <- factor(df$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))

df_lower <- as.data.frame(c(median_output_synthesizer$lower,
                            median_output_r0p15$lower,
                            median_output_r0p15_pw$lower))
names(df_lower) <- c("lower")
df_lower$Model <- c("Synthesizer", "Marginal", "Pairwise")
df_lower$Model <- factor(df_lower$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))


df_upper <- as.data.frame(c(median_output_synthesizer$upper,
                            median_output_r0p15$upper,
                            median_output_r0p15_pw$upper))
names(df_upper) <- c("upper")
df_upper$Model <- c("Synthesizer", "Marginal", "Pairwise")
df_upper$Model <- factor(df_upper$Model, levels = c("Synthesizer", "Marginal", "Pairwise"))

ggplot(df, aes(x = Model, y = estimate, color = Model)) + 
  geom_point(size=8, shape=4) +
  scale_colour_manual(values = cbPalette_nodatanosyn) +
  geom_errorbar(aes(ymin = df_lower$lower, ymax = df_upper$upper), 
                colour=cbPalette_nodatanosyn, width=.2, size = 1.5, position = position_dodge(0.01)) +
  theme_bw(base_size = 25) +
  theme(legend.position = "none") +
  ylab("Median Statistic") + xlab("") + ylim(92, 108) +
  geom_hline(yintercept = median_output_data$PE, linetype="solid", 
             color = "#999999", size=1) +
  geom_hline(yintercept = median_output_data$lower, linetype="dashed", 
             color = "#999999", size=1) +
  geom_hline(yintercept = median_output_data$upper, linetype="dashed", 
             color = "#999999", size=1)

#####################################################
##### ECDF utility metric ###########################
#####################################################

source('calculate_ECDF.R')

ECDF_r0p15_pw <- calculate_ECDF(orig_data, syndata = sim_syn_w_r0p15_pw, m = 20)
ECDF_r0p15_pw
