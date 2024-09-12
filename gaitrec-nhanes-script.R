library(data.table)
library(tidyverse)

data <- readRDS(file = "data/nhanes_fda_with_r.rds")
str(nhanes_data)

id_ind <- which(data$SEQN == 75111)

## extract subject level data and organize them into long format
df_wide <- data.frame(unclass(data[id_ind,unit]))
colnames(df_wide)[1:1440] <- 1:1440
df_long <- pivot_longer(df_wide, cols = 1:1440, names_to = "minute", values_to = "value")
df_long$minute <- as.numeric(df_long$minute)

p1 <- ggplot(data = df_long) +
  aes(x = minute, y = value) +
  geom_line() +
  scale_x_continuous(breaks = c(1,6,12,18,23)*60, 
                     labels = c("01:00","06:00","12:00","18:00","23:00")) +
  labs(x = "<------------- Full Day (24 hrs) ------------->", y = "MIMS",
       title = "(a) NHANES") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 15, hjust = 0.5))

# Read in and Reshape Data: -----------------------------------------------
gaitrec <- fread("https://springernature.figshare.com/ndownloader/files/22063200", )
gaitrec[1,]

gaitrec_df <- data.frame(grf = unlist(gaitrec[1, paste0("F_V_RAW_",1:193)]))
gaitrec_df$tind <- seq(0, nrow(gaitrec_df)-1, by =1)

Hz <- 250 # sampling rate
SI <- 1/Hz # sampling interval
SI_ms <- SI * 1000 # in millisecpnds
gaitrec_df$t_ms <- gaitrec_df$tind * SI_ms

p2 <- ggplot(gaitrec_df) +
  aes(x = t_ms , y = grf) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(y = "Ground Reaction Force (GRF)",
       x = "<------------- One Second (1000 ms) ------------->",
       title = "(b) GaitRec") +
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 15, hjust = 0.5))

ggpubr::ggarrange(p1, p2)

ggsave(filename = "PA-vs-mocap.pdf", device = "pdf")

