##############################################
# Script to visualize Rhythm ABR results
#
# author: Lara S. burchardt
##############################################

# 00: load packages   -----------------------------------------------------------

library(tidyverse)
library(base)
library(gridExtra)
library(latex2exp)

# 01: load data   ---------------------------------------------------------------

allspecies <- read_delim("combined_nov2020_scaledpeak_panama_allspecies.csv", delim = ";")

# laurita_new <- read_delim("Rhythm_ABR_laur_movmin_rms_integral.csv", delim= ";")
# cpersp_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_Rhythm_ABR_cper_movmin_RMS_09112020_forRanalysis.csv", delim = ";")
# drot_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Drot.csv", delim = ";")
# Gsor_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Gsor.csv", delim = ";")
# Mmol_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Mmol.csv", delim = ";")
# Mnig_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Mnig.csv", delim = ";")
# Phas_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Phas.csv", delim = ";")
# Ppar_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_Ppar.csv", delim = ";")
# Rnaso_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_rnaso.csv", delim = ";")
# Sbil_new <- read_delim("Rhythm_ABR_sbil_movmin_rms_integral.csv", delim = ";")
# Slep_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_slep.csv", delim = ";")
# Ttri_new <- read_delim("ABR_data/new analysis with rms/Rhythm_ABR_new_ttri.csv", delim = ";")

# 02: datamanagment ------------------------------------------------------------------

#what info do we want? difference between one rhythm and the next, avergage of each step over
# the different individuals or as a scatter plot
# xaxis: 6-8; 8-10; 10-12 [...];80-100 as categories
#yaxis: difference --> spread around 0, hopefully with a clear tendency

#scale to 0 added in excel every 6 Hz peak is shown as '0' everything else scaled to that (peak n -peak 1)
#mean peak per modrate

phas_mean<-Phas_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate,`scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean) %>% 
  #mutate( integral = integral * 1000) #Umrechnung von mV in mikroVolt
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

ppar_mean <- Ppar_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

Rnaso_mean <- Rnaso_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)#Umrechnung von mV in mikroVolt
  mutate(`scaled peak` = `scaled peak` *1000)

Sbil_mean <- allspecies %>%
  filter(species == "sbil") %>% 
  group_by(modrate) %>% 
  #select(modrate, `integral`) %>% 
  select(modrate, scaledpeak) %>% 
  #summarise_all(mean)
  summarise_all(median)

laurita_mean <- allspecies %>%
  filter(species == "laur") %>% 
  group_by(modrate) %>% 
  #select(modrate, `integral`) %>% 
  select(modrate, scaledpeak) %>% 
  #summarise_all(mean)
  summarise_all(median)

Cper_mean <- allspecies %>%
  filter(species == "cper") %>% 
  group_by(modrate) %>% 
  #select(modrate, `integral`) %>%
  select(modrate, scaledpeak) %>% 
  #summarise_all(mean)
  summarise_all(median)

Slep_mean <- Slep_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>%
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

Mnig_mean <- Mnig_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

Mmol_mean <- Mmol_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

Gsor_mean <- Gsor_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)#Umrechnung von mV in mikroVolt
  mutate(`scaled peak` = `scaled peak` *1000)

Ttri_mean <- Ttri_new %>%
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)#Umrechnung von mV in mikroVolt
  mutate(`scaled peak` = `scaled peak` *1000)

Drot_mean <- drot_new %>% 
  filter(rms_conf >= 0.95) %>% 
  group_by(modrate) %>% 
  select(modrate, `scaled peak`) %>% 
  #select(modrate,integral) %>%
  summarise_all(mean)%>% 
  #mutate( integral = integral * 1000)
  mutate(`scaled peak` = `scaled peak` *1000)#Umrechnung von mV in mikroVolt

#all data integral
allspecies_meanperspecies <- allspecies %>% 
  group_by(species, modrate) %>%  
  summarise_at(c("scaledpeak", "integral"), mean)

allspecies_medianperspecies <- allspecies %>% 
  group_by(species, modrate) %>%  
  summarise_at(c("scaledpeak", "integral"), median)


# all data mean

allspecies_mean <- allspecies %>% 
  group_by(modrate) %>% 
  summarise_at(c("scaledpeak", "integral"), mean)

# all data median 
  
allspecies_median <- allspecies %>% 
  group_by(modrate) %>% 
  summarise_at(c("scaledpeak", "integral"), median)

# Carollia mean, min & max, for detailed information figure 2

cper_mean_min_max <- allspecies %>% 
  filter(species == "cper") %>% 
  group_by(modrate) %>% 
  summarize_at("scaledpeak", c(mean, min, max))

#Sbil mean, min & max for correlation figure 4
sbil_mean_min_max <- allspecies %>% 
  filter(species == "sbil") %>% 
  group_by(modrate) %>% 
  summarize_at("scaledpeak", c(mean, min, max))

# Laurita mean, min & max for correlation figure 4

laur_mean_min_max <- allspecies %>% 
  filter(species == "laur") %>% 
  group_by(modrate) %>% 
  summarize_at("scaledpeak", c(mean, min, max))


# 03: Lineplots -----------------------------------------------------------------

lineplot1<-allspecies %>%
  filter(species == "cper") %>% 
  ggplot(aes(x=modrate, y= scaledpeak, color = ID))+
  geom_line()+
  #ggtitle("Carollia perspicillata")+
  scale_x_continuous(limits= c(6,100),breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
  xlab(b)+ 
  ylab(a)+
  theme_dark(base_size = 12)+
  theme(legend.position = "none")

# lineplot2<-data2 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
# ggtitle("Desmodus rotundus")
# 
# lineplot3<-data3 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
# ggtitle("Glossophaga soricina")
# 
# lineplot4<-data4 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_point()+
# ggtitle("Lonchorhina aurita")
# 
# 
# lineplot5<-data5 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
# ggtitle("Molossus molossus")
# 
# lineplot6<-data6 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_point()+
#   ggtitle("Myotis nigricans")
# 
# lineplot7<-data7 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
#   ggtitle("Phyllostomus hastatus")
# 
# lineplot8<-data8 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
#   ggtitle("Pteronotus parnellii")
# 
# lineplot9<-data9 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
#   ggtitle("Rhynchonycteris naso")
# 
# lineplot10<-  Sbil_new %>% 
#   ggplot(aes(x=modrate, y= integral, color= ID))+
#   geom_line()+
#   ggtitle("Saccopteryx bilineata")
#   
# 
# lineplot11<-data11%>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
#   ggtitle("Saccopteryx leptura")
# 
# 
# lineplot12<-data12 %>% 
#   ggplot(aes(x=modrate, y= peak, color= batid))+
#   geom_line()+
#   ggtitle("Thyroptera tricolor")

# 05: lineplots mean -------------------------------------------------
a<-list(title="MMS Integral [μV]",
        showticklabels= TRUE)
b<-list(title="Stimulus Presentation Rate [Hz] -- Rhythm Perception", 
        showticklabels = TRUE)


lineplot_m_1 <- cper_mean_min_max  %>% 
  #filter(modrate <= 30) %>% 
  #ggplot(aes(x=modrate, y=`integral` ))+
  ggplot(aes(x=modrate, y= fn1 ))+
  geom_line(color= "white")+
  geom_pointrange(aes(ymin=fn2, ymax=fn3), color = "darkblue")+
  geom_point(shape= 15, color = "darkblue")+
  scale_x_continuous(limits= c(0,100),breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
  xlab(b)+ 
  ylab(a)+
  #ggtitle(expression(italic("Carollia perspicillata")))+
  theme_dark(base_size = 12)

lineplot_m_2 <- sbil_mean_min_max  %>% 
  #filter(modrate <= 30) %>% 
  #ggplot(aes(x=modrate, y=`integral` ))+
  ggplot(aes(x=modrate, y= fn1 ))+
  geom_line(color= "white")+
  geom_pointrange(aes(ymin=fn2, ymax=fn3), color = "darkblue")+
  geom_point(shape= 15, color = "darkblue")+
  scale_x_continuous(limits= c(0,100),breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
  xlab(b)+ 
  ylab(a)+
  #ggtitle(expression(italic("Saccopteryx bilineata")))+
  theme_dark(base_size = 12)

lineplot_m_3 <- laur_mean_min_max  %>% 
  #filter(modrate <= 30) %>% 
  #ggplot(aes(x=modrate, y=`integral` ))+
  ggplot(aes(x=modrate, y= fn1 ))+
  geom_line(color= "white")+
  geom_pointrange(aes(ymin=fn2, ymax=fn3), color = "darkblue")+
  geom_point(shape= 15, color = "darkblue")+
  scale_x_continuous(limits= c(0,100),breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
  xlab(b)+ 
  ylab(a)+
  #ggtitle(expression(italic("Lonchorina aurita")))+
  theme_dark(base_size = 12)


# lineplot_m_2 <- Drot_mean %>% 
#   #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#   geom_line(color = "white")+
#   geom_point(shape= 15, color = "darkblue")+
#   ggtitle(expression(italic("Desmodus rotundus")))+
#   scale_x_continuous(limits= c(0,100),breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#   #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#   scale_y_continuous(limits = c(-30, 2))+
#   xlab(b)+ 
#   ylab(a)+
#   theme_dark(base_size = 14)
#   
#   lineplot_m_3 <- Gsor_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle(expression(italic("Glossophaga soricina")))+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-0.02, 0.01))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark()
#   
#   lineplot_m_4<-laurita_mean %>% 
#     #filter(modrate <= 30) %>% 
#   #ggplot(aes(x=modrate, y= `integral`))+
#     ggplot(aes(x=modrate, y= scaledpeak ))+
#     geom_line(color= "white")+
#     ggtitle(expression(italic("Lonchorhina aurita")))+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     #scale_y_continuous(limits = c(-60, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14)
#   
#   
#   lineplot_m_5<- Mmol_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color = "white")+
#     ggtitle(expression(italic("Molossus molossus")))+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-30, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14)
#   
#   lineplot_m_6<- Mnig_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle(expression(italic("Myotis nigricans")))+
#     geom_point(shape= 15, color= "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-30, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14)
#   
#   lineplot_m_7<-phas_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle(expression(italic("Phyllostomus hastatus")))+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-30, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14)
#   
# 
#   lineplot_m_8<-ppar_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle("Pteronotus parnellii")+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-0.02, 0.01))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark()
#   
#   
#   lineplot_m_9 <- Rnaso_mean %>%
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle("Rhynchonycteris naso")+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-0.02, 0.01))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark()
#   
#   lineplot_m_10 <- Sbil_mean %>% 
#     #filter(modrate <= 30) %>% 
#     #ggplot(aes(x=modrate, y= `integral`))+
#     ggplot(aes(x=modrate, y=scaledpeak ))+
#     geom_line(color = "white")+
#     ggtitle(expression(italic("Saccopteryx bilineata")))+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     #scale_y_continuous(limits = c(-30, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14)
# 
#                    
#   
#   
#   lineplot_m_11 <- Slep_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color="white")+
#     ggtitle(expression(italic("Saccopteryx leptura")))+
#     geom_point(shape= 15, color= "darkblue")+
#     scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     #scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30))+
#     scale_y_continuous(limits = c(-30, 2))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark(base_size = 14, )
#   
#   lineplot_m_12<- Ttri_mean %>% 
#     #filter(modrate <= 30) %>% 
#   ggplot(aes(x=modrate, y= `scaled peak`))+
#     geom_line(color= "white")+
#     ggtitle("Thyroptera tricolor")+
#     geom_point(shape= 15, color = "darkblue")+
#     scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
#     scale_y_continuous(limits = c(-0.02, 0.01))+
#     xlab(b)+ 
#     ylab(a)+
#     theme_dark()
  

  lineplot_m_13<- allspecies_median %>% 
    #filter(modrate <= 30) %>% 
    ggplot(aes(x=modrate, y= scaledpeak))+
    geom_line(color= "white")+
    #ggtitle("all 12 bat species testes, scaled integral")+
    geom_point(shape= 15, color = "darkblue")+
    scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    #scale_y_continuous(limits = c(-0.02, 0.01))+
    xlab(b)+ 
    ylab(a)+
    theme_dark()+
    theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12))
  #with font: Times new roman 
  #theme(legend.position = "none", axis.title=element_text(size=16,face="bold", family = "serif"), axis.text = element_text(size = 12, family = "serif))
  
  lineplot_m_14<- allspecies_meanperspecies %>% 
    #filter(modrate <= 30) %>% 
    ggplot(aes(x=modrate, y= scaledpeak, colour = species, fill = species))+
    geom_line(color = "white")+
    #ggtitle("all 12 bat species testes, scaled integral")+
    geom_point(shape= 15)+
    scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    #scale_y_continuous(limits = c(-0.02, 0.01))+
    xlab(b)+ 
    ylab(a)+
    theme_dark()+
    theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12))
  
  lineplot_m_15<- allspecies_meanperspecies %>% 
    #filter(modrate <= 30) %>% 
    ggplot(aes(x=modrate, y= integral, colour = species, fill = species))+
    geom_line(color = "white")+
    #ggtitle("all 12 bat species testes, scaled integral")+
    geom_point(shape= 15)+
    scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    #scale_y_continuous(limits = c(-0.02, 0.01))+
    xlab(b)+ 
    ylab(a)+
    theme_dark()+
    theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12))
  
  lineplot_m_16 <- ggplot()+
    geom_line(data = laurita_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = laurita_mean, aes(x = modrate, y = `scaled peak`, colour = 'laurita_mean'), shape= 15, color = "darkblue")+
    geom_line(data = Cper_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = Cper_mean, aes(x = modrate, y = `scaled peak`, shape = 16, colour = 'Cper_mean'), shape = 16 ,color= "black")+
    geom_line(data = Sbil_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = Sbil_mean, aes(x = modrate, y = `scaled peak`), shape= 15, color = "darkorange")+
    geom_line(data = Slep_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = Slep_mean, aes(x = modrate, y = `scaled peak`), shape = 16 ,color= "darkred")+
    geom_line(data = phas_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = phas_mean, aes(x = modrate, y = `scaled peak`), shape= 15, color = "darkgreen")+
    geom_line(data = Drot_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = Drot_mean, aes(x = modrate, y = `scaled peak`), shape = 16 ,color= "darkgrey")+
    geom_line(data = Mmol_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = Mmol_mean, aes(x = modrate, y = `scaled peak`), shape= 15, color = "skyblue")+
    geom_line(data = Mnig_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = Mnig_mean, aes(x = modrate, y = `scaled peak`), shape = 16 ,color= "yellow")+
    geom_line(data = Gsor_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = Gsor_mean, aes(x = modrate, y = `scaled peak`), shape= 15, color = "white")+
    geom_line(data = ppar_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = ppar_mean, aes(x = modrate, y = `scaled peak`), shape = 16 ,color= "purple")+
    geom_line(data = Rnaso_mean, aes(x = modrate, y = `scaled peak`), color = "white")+
    geom_point(data = Rnaso_mean, aes(x = modrate, y = `scaled peak`), shape= 15, color = "green")+
    geom_line(data = Ttri_mean, aes(x = modrate, y = `scaled peak`), color= "white")+
    geom_point(data = Ttri_mean, aes(x = modrate, y = `scaled peak`), shape = 16 ,color= "red")+
    scale_x_continuous(breaks= c(6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    xlab(b)+ 
    ylab(a)+
    theme_dark(base_size = 14)
  
  # histogram echolocation production 
  
  hist_data_ioi <- read_delim("rhythm_ioi_abr_species.csv", delim = ";")
  hist_data_fft <- read_delim("rhythm_fft_sbil_cper_laur_forABR.csv", delim = ";")
  
  #c.persp
  hist_1 <- ggplot(data = subset(hist_data_fft, !is.na(rhythm_fft_cper)), aes(x = rhythm_fft_cper))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "white", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 12)
  
  #d.rotundus
  hist_2 <- ggplot(data = hist_data, aes(x = rhythm_ioi_drot))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "darkblue", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 12)
  
  #l.aurita
  hist_4 <- ggplot(data = hist_data_fft, aes(x = rhythm_fft_laur, na.rm = TRUE))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "white", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 12)
  
  #P. hastatus
  hist_7 <- ggplot(data = hist_data, aes(x = rhythm_ioi_phas))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "darkblue", fill = "darkblue")+    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 14)
  
  #S. bilineata
  hist_10 <- ggplot(data = hist_data_fft, aes(x = rhythm_fft_sbil, na.rm = TRUE))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "white", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 12)
  
  #S.leptura
  hist_11 <- ggplot(data = hist_data, aes(x = rhythm_ioi_slep, na.rm = TRUE))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 2,  stat = "bin", bins = 4, color = "darkblue", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 14)
  
  #M.molossus
  
  hist_5 <- ggplot(data = hist_data, aes(x = rhythm_ioi_mmol, na.rm = TRUE))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "darkblue", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 14)
  #M.nigricans
  
  hist_6 <- ggplot(data = hist_data, aes(x = rhythm_ioi_mnig, na.rm = TRUE))+
    geom_histogram(aes(y=stat((count)/sum(stat(count))*100)), binwidth = 5,  stat = "bin", bins = 4, color = "darkblue", fill = "darkblue")+
    scale_x_continuous(limits= c(0,100), breaks= c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,40,60,80,100))+
    ylab("Percentage[%]")+
    xlab("Exact Beat Frequencies [Hz] -- Rhythm Production")+
    theme_dark(base_size = 14)
  
  #combined Plots
  #c.perspicillata
  cowplot::plot_grid(lineplot_m_1, hist_1, align = "v", ncol = 1, rel_heights = c(0.7, 0.3))
  #d.rotundus
  #cowplot::plot_grid(lineplot_m_2, hist_2, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  #p. hastatus
  #cowplot::plot_grid(lineplot_m_7, hist_7, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  #s. bilineata
  cowplot::plot_grid(lineplot_m_2, hist_10, align = "v", ncol = 1, rel_heights = c(0.7, 0.3))
  #l. aurita
  cowplot::plot_grid(lineplot_m_3, hist_4, align = "v", ncol = 1, rel_heights = c(0.7, 0.3))
  #s.leptura
  #cowplot::plot_grid(lineplot_m_11, hist_11, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  # m.molossus
  #cowplot::plot_grid(lineplot_m_5, hist_5, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  # m.nigricans
  #cowplot::plot_grid(lineplot_m_6, hist_6, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
  
  
  #Boxplots
  box_sbil <- Sbil_new %>% 
  ggplot(aes())+
    #geom_violin()+
    geom_boxplot(aes(x= modrate, y= integral, fill = modrate))+
    scale_fill_manual(values=c("#33CCCC"))+
    #scale_y_continuous(limits = c(40,260))+
    geom_jitter(aes(x= modrate), shape = 1, size = 0.6)
  