#setwd() <- Set at the top level directory where the PAO package is located (ie. (../PAO/)
setwd("..")
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(reshape2)
require("PAO")

"Set Parametes"
fixed_params <- c(s=100, tt=20/252, r = 0.05, m = 20, numsim = 10^4)
Strikes <- c(90, 100, 110)
Vol <- c(0.20, 0.40, 0.80, 1.2, 2.4)
Methods <- c("AO", "AOECO", "AOG", "AOAV")
for (i in Strikes){for (j in Vol){print(j)}}

"Creating Row names"
R_N <-rep(Strikes,each=2)
R_N[seq(from=1,to =6,by =2)] <- c(paste(R_N[seq(from=1,to =6,by =2)],"Price"))
R_N[seq(from=2,to =7,by =2)] <- c(paste(R_N[seq(from=2,to =7,by =2)],"SD"))
R_N

"Creating Column Names in a round-about  way"
C_N <- list()
sum = 0
for (i in Vol){for (j in Methods){
  sum = sum + 1
  C_N[sum] <- paste(i,j)
  }}

"Create Output Matix"
outputs <- matrix(nrow = 6, ncol = 20)
colnames(outputs) <- C_N
rownames(outputs) <- R_N
outputs[,1]

"Fill the Matrix with simulation runs"
counter1 = 0
counter2 = 0
for (i in Strikes){
  #keeps track of Strike in Loop
  counter1 = counter1 + 2
  #Reset to 1st row once all 20 rows are filled
  counter2 = 1
  for (j in Vol){
    ACO_tmp <- PAO::ACO(s= 100, k= i, v = j, tt = 20/252, r = 0.05, m = 20, numsim = 10^5)
    outputs[counter1-1,counter2] <- ACO_tmp[1]
    outputs[counter1,counter2] <- ACO_tmp[2]
    ACO_CV_ECO_tmp <- PAO::ACO_CV_ECO(s= 100, k= i, v = j, tt = 20/252, r = 0.05, m = 20, numsim = 10^5)
    outputs[counter1-1,counter2+1] <- ACO_CV_ECO_tmp[1]
    outputs[counter1,counter2+1] <- ACO_CV_ECO_tmp[2]
    ACO_CV_GEOM_tmp <- PAO::ACO_CV_GEOM(s= 100, k= i, v = j, tt = 20/252, r = 0.05, m = 20, numsim = 10^5)
    outputs[counter1-1,counter2+2] <- ACO_CV_GEOM_tmp[1]
    outputs[counter1,counter2+2] <- ACO_CV_GEOM_tmp[2]
    ACO_AV_tmp <- PAO::ACO_AV(s= 100, k= i, v = j, tt = 20/252, r = 0.05, m = 20, numsim = 10^5)
    outputs[counter1-1,counter2+3] <- ACO_AV_tmp[1]
    outputs[counter1,counter2+3] <- ACO_AV_tmp[2]
    #Ensure that after 1st set of vol values are filled, move to next vol values
    counter2 = counter2 + 4
  }}
"Transpose Data to make it easier to read"
df <- as.data.frame(t(outputs))
df
"Apply rownames"
df <- cbind(index = rownames(df), df)
rownames(df) <- 1:nrow(df)
"Split Methodology and Volatility for ggplot2"
df <- df %>%
  separate(index, c("index", "method"), sep=" ")

"Seperate Sd and Prices"
df_price <- select(df, -c('90 SD', '100 SD', '110 SD'))
df_sd <- select(df, -c('90 Price', '100 Price', '110 Price'))
#Order the Dataframes to make it easier for geom_path
df_sd <- df_sd[order(df$method),]
df_price <- df_price[order(df$method),]
  

price_90 <- df_price %>% tidyr::gather("id", "value", 3) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+ theme(legend.direction="horizontal") +
  geom_path() +labs(x ="Volatility", y = "Option Price")

price_100 <- df_price %>% tidyr::gather("id", "value", 4) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+ theme(axis.title.y = element_blank()) +
  geom_path() +labs(x ="Volatility")

price_110 <- df_price %>% tidyr::gather("id", "value", 5) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+ theme(axis.title.y = element_blank()) +
  geom_path() +labs(x ="Volatility") 

sd_90 <- df_sd %>% tidyr::gather("id", "value", 3) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+ theme(legend.direction="horizontal",
                      axis.title.x=element_blank()) +
  geom_path() +labs(title="Strike of 90", y = "Standard Deviation", color = "Pricing Method") +
  scale_color_hue(labels = c("Asian Call Option", "Antithetical Variable", "European Call Control Variate", "Geometic Control Variate")) +
  theme(plot.title = element_text(hjust = 0.5))

sd_100 <- df_sd %>% tidyr::gather("id", "value", 4) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+
  geom_path() + theme(axis.title.y = element_blank(),
                      axis.title.x=element_blank()) +labs(title="Strike of 100") +
  theme(plot.title = element_text(hjust = 0.5))

sd_110 <- df_sd %>% tidyr::gather("id", "value", 5) %>%
  ggplot(., aes(index, value, color = method, group= method)) +
  geom_point()+
  geom_path() + theme(axis.title.y = element_blank(),
                      axis.title.x=element_blank())+labs(title="Strike of 110") +
  theme(plot.title = element_text(hjust = 0.5))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sd_90)

p3 <- grid.arrange(arrangeGrob(sd_90 + theme(legend.position="none"),
                               sd_100 + theme(legend.position="none"),
                               sd_110 + theme(legend.position="none"),
                               price_90 + theme(legend.position="none"),
                               price_100 + theme(legend.position="none"),
                               price_110 + theme(legend.position="none"),
                               nrow=2),
                   mylegend, nrow=2,heights=c(10, 1))

