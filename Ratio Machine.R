library(tidyverse)

#Input directory and filename to assign data to variable, "samples".
setwd("directory")
samples <- as.data.frame(read.csv("filename", sep = ",", dec = ".", header = TRUE, na = "NA", stringsAsFactors = FALSE))

#Part I: Find significant ratios

# Select the numerator and denominator in the order of ratios you want to calculate. The names must
# be the same as in the file. Note any invalid characters will be assigned a ".". For multiple
# analytes simply add them after the first one ex. c('analyte 1', 'analyte 2',...). The example shows
# ratios of analyte 1/4 and (2+3)/5
numerators <- list('analyte 1', c('analyte 2', 'analyte 3'))
denominators <- list('analyte 4', 'analyte 5')

ratio_names <- c()
all_WT_ratios <- c()
all_KO_ratios <- c()
mean_WT_ratios <- c()
sd_WT_ratios <- c()
mean_KO_ratios <- c()
sd_KO_ratios <- c()
p_values <- c()

for (i in 1:length(denominators)){
  # Stores ratio name
  numerator <- unlist(numerators[i])
  denominator <- unlist(denominators[i])
  numerator_name <- paste(numerator, collapse = '+')
  denominator_name <- paste(denominator, collapse = '+')
  ratio_name <- paste(numerator_name, denominator_name, sep = "/")
  ratio_names <- c(ratio_names, ratio_name)
  
  # Calculates the ratio for all samples within the group "WT". Group name can be changed from
  # "WT" to a different name if comparing different groups.
  WT_rows <- which(samples$Group == "WT")
  WT_ratios <- c()
  for (i in WT_rows){
    WT_numerator <- sum(select(samples[i, ], numerator))
    WT_denomiator <- sum(select(samples[i, ], denominator))
    WT_ratio <- WT_numerator / WT_denomiator
    WT_ratios <- c(WT_ratios, WT_ratio)
  }
  all_WT_ratios <- as.data.frame(cbind(all_WT_ratios, WT_ratios))
  mean_WT_ratio <- mean(WT_ratios)
  mean_WT_ratios <- c(mean_WT_ratios, mean_WT_ratio)
  sd_WT_ratio <- sd(WT_ratios)
  sd_WT_ratios <- c(sd_WT_ratios, sd_WT_ratio)
  
  # Calculates the ratio for all samples within the group "KO".
  KO_rows <- which(samples$Group == "KO")
  KO_ratios <- c()
  for (i in KO_rows){
    KO_numerator <- sum(select(samples[i, ], numerator))
    KO_denomiator <- sum(select(samples[i, ], denominator))
    KO_ratio <- KO_numerator / KO_denomiator
    KO_ratios <- c(KO_ratios, KO_ratio)
  }
  all_KO_ratios <- as.data.frame(cbind(all_KO_ratios, KO_ratios))
  mean_KO_ratio <- mean(KO_ratios)
  mean_KO_ratios <- c(mean_KO_ratios, mean_KO_ratio)
  sd_KO_ratio <- sd(KO_ratios)
  sd_KO_ratios <- c(sd_KO_ratios, sd_KO_ratio)
  
  # Tests for significance between the groups and stores p value for each ratio.
  t_test <- t.test(KO_ratios, WT_ratios)
  p_value <- t_test$p.value
  p_values <- c(p_values, p_value)
}

names(all_WT_ratios) <- ratio_names
names(all_KO_ratios) <- ratio_names

# Calculates p.adj
p_adj <- round(p.adjust(p_values, method = "BH"), 4)
p_values <- round(p_values, 4)
#Formats data and displays it
data <- data.frame(Ratio = ratio_names, WT_ratio = round(mean_WT_ratios, 3), KO_ratio = round(mean_KO_ratios, 3), p = p_values, p.adj = p_adj)
sig_data <- data[data$p.adj < 0.05, ]
sig_data[order(sig_data$p.adj), ]

#Part II: Plot significant ratios

#User inputs desired row number based on output from Part I
num_row <- 12
ratio_name <- data$Ratio[num_row]

# Calculates the mean ratio and standard deviation of each group
mean_WT_ratio <- mean_WT_ratios[num_row]
sd_WT_ratio <- sd_WT_ratios[num_row]
mean_KO_ratio <- mean_KO_ratios[num_row]
sd_KO_ratio <- sd_KO_ratios[num_row]

WT_ratios <- all_WT_ratios[ ,num_row]
KO_ratios <- all_KO_ratios[ ,num_row]

#Creates label based on p.adj
p.adj <- data$p.adj[num_row]
if (p.adj > 0.05){
  stars = "ns"
}
if (p.adj < 0.05){
  stars = "*"
}
if (p.adj < 0.01){
  stars = "**"
}
if (p.adj < 0.001){
  stars = "***"
}

# Formats the data for proper plotting
df <- data.frame(Group=c("WT", "KO"),
                 Ratio=c(mean_WT_ratio, mean_KO_ratio),
                 Stdev=c(sd_WT_ratio, sd_KO_ratio))
top_y <- max(df$Ratio)+ max(df$Stdev)
line <- tibble(x=c("WT", "WT", "KO", "KO"), y=c(top_y+0.1, top_y+0.2, top_y+0.2, top_y+0.1))
df$Group <- factor(df$Group, levels = df$Group)

#Automatically plots data
tiff("filename", res = 300, height = 5, width = 5, units = "in")
ggplot(data=df, aes(x=Group, y=Ratio)) +
  geom_col(width=0.4, fill="orange") +
  geom_errorbar(aes(ymin = Ratio - Stdev, ymax = Ratio + Stdev), width=.1) +
  geom_line(data = line, aes(x = x, y = y, group = 1)) +
  geom_point(data = data.frame(WT_ratios, Group = rep("WT", length(WT_ratios))), aes(x=Group,y=WT_ratios, fill="black"), position = position_jitterdodge(jitter.width = 0.1)) +
  geom_point(data = data.frame(KO_ratios, Group = rep("KO", length(KO_ratios))), aes(x=Group,y=KO_ratios, fill="black"), position = position_jitterdodge(jitter.width = 0.1)) +
  guides(fill="none") +
  annotate("text", x = 1.5, y = top_y + 0.25, label = stars, size = 8) +
  xlab(NULL) +
  ylab(ratio_name) +
  scale_y_continuous(limits = c(0, top_y + 1), expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(), 
        axis.line = element_line(color = "#3D4852"),
        axis.ticks = element_line(color = "#3D4852")) 

dev.off()
