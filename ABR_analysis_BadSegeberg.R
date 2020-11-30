# Intro -------------------------------------------------------
# Lara S. Burchardt
# Rhythm ABR analysis, data from Bad Segeberg June 2020 & July 2020
# 10 female, 10 males measured
# 10 short and 10 long Isolation calls used as stimuli
# preanalysis in Matlab: moving minimum substraction (Källstrand et al, 2014) with subsequent trapezoidal integration over first 10 ms;
# root-mean-square and std calculations in original data and 500 resamples, significant results have <95% confidence, meaning rms and std
# of original data needing to be higher than from resamples 
# Preanalysis was done in Matlab
# Additionally original data was scaled, 6Hz results as baseline this was done in manually in Excel 

#00: Packages ------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(gridExtra)

# 01: Data ----------------------------------------------------------------


Cper_BadSegeberg <- read_delim("Rhythm_ABR__BS_sigsubset_movmin_RMS_09112020.csv", delim = ";")
Cper_panama <- read_delim("Rhythm_ABR_cper_movmin_RMS_09112020.csv", delim = ";")


#### Main datasets for plots

# only 6, 24, and 40 Hz from Panama Data do mimic Bad Segeberg Data 
# only C. perspicillata males were measured in Panama
Cper_panama <- Cper_panama %>% 
  filter(modrate == 6 | modrate == 24 | modrate == 40) %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  mutate(modrate = as.factor(modrate),
         sex = "male")

# Bad Segeberg data, significant results, only short stimuli
conf_over_sig_short <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  mutate(modrate = as.factor(modrate))

# Bad Segeberg data, significant results, only long stimuli
conf_over_sig_long <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long") %>% 
  mutate(modrate = as.factor(modrate))

# Bad Segeberg data, significant results

conf_over_sig <- Cper_Bad Segeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  


##### secondary datasets, more detail, more comparisson
  
  # Bad Segeberg data, significant results, only short stimuli, mean by ID
  conf_over_sig_short_perID <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  mutate(modrate = as.factor(modrate)) %>% 
  group_by(modrate, ID, sex) %>% 
  summarise_at("integral", median)

# Bad Segeberg data, significant results, only long stimuli, , mean by ID
conf_over_sig_long_perID <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long") %>% 
  mutate(modrate = as.factor(modrate)) %>% 
  group_by(modrate, ID, sex) %>% 
  summarise_at("integral", median)
  
  
  
  
###### 3. level of detail   
conf_over_sig_short_male <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  filter(sex == "male") %>% 
  mutate(modrate = as.factor(modrate))

conf_over_sig_short_female <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  filter(sex == "fem") %>% 
  mutate(modrate = as.factor(modrate))

conf_over_sig_long_male <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long") %>% 
  filter(sex == "male")

conf_over_sig_long_female <- Cper_BadSegeberg %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long") %>% 
  filter(sex == "fem")

conf_over_sig_short_6 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  filter(modrate == 6)

conf_over_sig_short_25 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  filter(modrate == 25)

conf_over_sig_short_44 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC short") %>% 
  filter(modrate == 44)

conf_over_sig_long_6 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long") %>% 
  filter(modrate == 6)

conf_over_sig_long_25 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long")%>% 
  filter(modrate == 25)

conf_over_sig_long_44 <- female_male_short_long %>% 
  filter(rms_conf >= 0.95 & std_conf >= 0.95) %>% 
  filter(typ == "IC long")%>% 
  filter(modrate == 44)

# 02: Summary statistics -------------------------------------------------

integral_median <- median(conf_over_sig_long$integral, na.rm = TRUE)
rms_median <- median(conf_over_sig_long$rms, na.rm = TRUE)

# 03: Plots ---------------------------------------------------------------

 #combination of data from panama, short and long natural stimuli, male and female
 # subsets of data that are necessary: sub1-5
 # resulting dataset: data_comparison_P_BS 
 #resulting plot: p4, lineplot with 5 parameter variations
 
 sub1 <- allspecies_medianperspecies %>% 
   filter(species <= "cper") %>% 
   filter( modrate == 6 | modrate == 24 | modrate == 40 ) %>% 
   mutate(type = "Panama", sex = "male") %>% 
   select(type, modrate, integral, sex)
 
 sub2 <- conf_over_sig_short %>% 
   group_by(modrate, sex) %>% 
   #summarise_at("integral", mean) %>%
   summarise_at("integral", median) %>%
   filter(sex == "male") %>% 
   mutate(type = "BS_short_male", species = "cper", modrate = as.numeric(as.character(modrate))) 
 
sub3 <- conf_over_sig_short %>% 
     group_by(modrate, sex) %>% 
     #summarise_at("integral", mean) %>% 
    summarise_at("integral", median) %>%
     filter(sex == "fem") %>% 
   mutate(type = "BS_short_female", species = "cper", modrate = as.numeric(as.character(modrate))) 
   
   
 sub4 <- conf_over_sig_long %>% 
   group_by(modrate, sex) %>% 
   #summarise_at("integral", mean) %>%
   summarise_at("integral", median) %>%
   filter(sex== "male") %>% 
   mutate(type = "BS_long_male", species = "cper", modrate = as.numeric(as.character(modrate)))
   
 sub5 <- conf_over_sig_long %>% 
   group_by(modrate, sex) %>% 
   #summarise_at("integral", mean) %>%
   summarise_at("integral", median) %>%
   filter(sex== "fem") %>% 
   mutate(type = "BS_long_female", species = "cper", modrate = as.numeric(as.character(modrate)))
 
data_comparison_P_BS <- rbind(sub1, sub2, sub3, sub4, sub5) 
data_comparison_BS_short <- rbind(sub2, sub3)
data_comparison_BS_long<- rbind(sub4, sub5)




# lab titles
a<-list(title="MMS Integral [μV]",
        showticklabels= TRUE)
b<-list(title="Presentation Rate [Hz]", 
        showticklabels = TRUE)

# subplots 

p5 <- data_comparison_BS_short %>%  
  ggplot(aes(x=modrate, y= integral, colour = type, fill = type))+
  geom_line(color = "white")+
  ggtitle("Natural Stimuli: 2ms")+
  geom_point(shape= 15)+
  scale_x_continuous(breaks= c(6,25,44))+
  scale_y_continuous(limits = c(20, 1200))+
  xlab(b)+ 
  ylab(a)+
  theme_dark()+
  theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))

p6 <- data_comparison_BS_long %>%  
  ggplot(aes(x=modrate, y= integral, colour = type, fill = type))+
  geom_line(color = "white")+
  ggtitle("Natural Stimuli: 8ms")+
  geom_point(shape= 15)+
  scale_x_continuous(breaks= c(6,25,44))+
  scale_y_continuous(limits = c(20, 1200))+
  scale_color_hue(labels = c("Female", "Male"))+
  guides(color=guide_legend("Sex"))+
  xlab(b)+ 
  ylab(a)+
  theme_dark()+
theme(axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))

p7 <- sub1 %>% 
  ggplot(aes(x=modrate, y= integral, colour = type, fill = type))+
  geom_line(color = "white")+
  ggtitle("Artificial Stimuli")+
  geom_point(shape= 15, color = "#33CCCC")+
  scale_fill_manual(values=c("#33CCCC"))+
  scale_x_continuous(breaks= c(6,24,40))+
  scale_y_continuous(limits = c(20, 1200))+
  xlab(b)+ 
  ylab(a)+
  theme_dark()+
  theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))

p4 <- data_comparison_P_BS %>% 
  ggplot(aes(x=modrate, y= integral, colour = type, fill = type))+
  geom_line(color = "white")+
  ggtitle("ABR response of C. perspicillata to artificial and natural stimuli")+
  geom_point(shape= 15)+
  scale_x_continuous(breaks= c(6,24,25,40,44))+
  scale_y_continuous(limits = c(20, 1200))+
  xlab(b)+ 
  ylab(a)+
  theme_dark()
 

p0 <- Cper_panama %>% 
  ggplot(aes(x= modrate, y = integral, fill = sex))+
  #geom_violin()+
  geom_boxplot()+
  scale_fill_manual(values=c("#33CCCC"))+
  scale_y_continuous(limits = c(20,1200))+
  #geom_jitter(shape = 16, size = 1.5, color = "black")+
  geom_jitter(shape = 1, size = 0.6)+
  labs(title="Exp1: Artificial Stimuli",x="Stimulus Presentation Rate [Hz]", y= "MMS Integral [µV]")+
  theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))
 


p1 <- conf_over_sig_short_perID %>% 
  group_by(ID) %>% 
  ggplot(aes(x=modrate, y=integral, fill = sex)) + 
  #geom_violin()+
  geom_boxplot()+
  scale_fill_manual(values=c("#FF6666", "#33CCCC" ))+
  scale_y_continuous(limits = c(20,1200))+
  geom_jitter(shape = 1, size = 0.6)+
  labs(title="Exp2: Natural Stimuli, 2 ms",x="Stimulus Presentation Rate [Hz]", y= "MMS Integral [µV]")+
  theme(legend.position = "none", axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))


p2 <- conf_over_sig_long_perID %>% 
  group_by(ID) %>% 
  ggplot(aes(x=modrate, y=integral, fill = sex)) + 
  geom_boxplot()+
  scale_y_continuous(limits = c(20,1200))+
  geom_jitter(shape= 1, size= 0.6)+
  labs(title="Exp2: Natural Stimuli, 8 ms",x="Stimulus Presentation Rate [Hz]", y= "MMS Integral [µV]")+
  scale_fill_manual(name = "Sex",
                    labels = c("Female", "Male"),
                    values=c("#FF6666", "#33CCCC" ))+
  theme(axis.title=element_text(size=16,face="bold"), axis.text = element_text(size = 12), plot.title=element_text(size=16, face="bold"))


cowplot::plot_grid(p0,p1,p2, align = "v", ncol = 3, rel_heights =  c(0.5, 0.5), rel_widths = c(0.3,0.3,0.4))
cowplot::plot_grid(p7,p5,p6, align = "v", ncol = 3, rel_heights =  c(0.5, 0.5), rel_widths = c(0.3,0.3,0.4))

# 03: Tests ----------------------------------------------------

cor.test(conf_over_sig_long$modrate, conf_over_sig_long$integral) #r = -0.16, p = 0.003
cor.test(conf_over_sig$modrate, conf_over_sig$rms) #r = -0.012, P = 0.8

# t.test, are two of the modrates sig. different? only for long stimuli (p=<0.00)
t.test(conf_over_sig_long_6$integral,conf_over_sig_long_44$integral)
