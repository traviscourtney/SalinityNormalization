##############Load Required Libraries##############
library(tidyverse)
library(foreign)
library(patchwork)
library(fs)
library(broom)
library(lmodel2)
library(maps)

##############Construct Conceptual Diagram##############
sal=seq(0,40,0.1) #sequence of salinity values
TA_0=sal*(2300/35) #mixing line between TA=0@S=0 and TA=2300@S=35
TA_1500=sal*(2300-1500)/35+1500 #mixing line between TA=1500@S=0 and TA=2300@S=35

NEC=60 #arbitrary TA drawdown by net ecosystem calcification (NEC)

S_reef=32 #salinity of the reef
TA_reef=(S_reef*(2300-1500)/35+1500)-NEC #resulting TA of the reef based on mixing between TA=1500@S=0 and TA=2300@S=35 and NEC
TA_mixing=S_reef*(2300-1500)/35+1500 #resulting TA of the reef based on mixing between TA=1500@S=0 and TA=2300@S=35

TA_calc_no_mix=(TA_reef-1500)*35/S_reef+1500 #normalize TA reef to offshore salinity assuming mixing between TA=1500@S=0 and TA=2300@S=35
TA_1500_calc=sal*(TA_calc_no_mix-1500)/35+1500 #generate salinity mixing line of normalized reef TA

ocean=as.data.frame(cbind(rbind(S_reef,35),rbind(TA_reef,2300))) #assign ocean TA and S
colnames(ocean)=c("sal","ta") #assign column names

hypothetical=as.data.frame(cbind(sal,TA_0,TA_1500,TA_1500_calc)) #generate hypothetical dataframe of all simulated TA points

#generate hypothetical mixing and calcification plots
a_plot=ggplot()+
  geom_point(data=ocean,aes(x=sal,y=ta-1000),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_0),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500),size=1)+
  #annotate("text", x=31.15, y=2390, size=6,hjust=0,label= "(a) Unknown TA @ S=0") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  ylab("TA (µmol/kg)")+xlab("Salinity (g/kg)")+
  theme_classic()+theme(text = element_text(size=20))

c_plot=ggplot()+
  geom_point(data=ocean,aes(x=sal,y=ta),size=5)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_0),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500),size=1)+
  #annotate("text", x=31.15, y=2390, size=6,hjust=0,label= "(a) Unknown TA @ S=0") +
  scale_x_continuous(expand = c(0, 0), limits = c(31, 36)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(2000, 2400)) +
  ylab("TA (µmol/kg)")+xlab("Salinity (g/kg)")+
  theme_classic()+theme(text = element_text(size=20))

mixing=bind_cols(sal=rbind(S_reef,S_reef,35,35),ta=rbind(TA_reef,TA_mixing,TA_calc_no_mix,2300))

b_plot=ggplot()+
  geom_point(data=mixing,aes(x=sal,y=ta-1000),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500_calc),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500),size=1)+
  #annotate("text", x=31.15, y=2390, size=6,hjust=0,label= "(b) Unknown Order of Mixing") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  ylab("TA (µmol/kg)")+xlab("Salinity (g/kg)")+
  theme_classic()+theme(text = element_text(size=20))

d_plot=ggplot()+
  geom_point(data=mixing,aes(x=sal,y=ta),size=5)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500_calc),size=1)+
  geom_line(data=hypothetical,aes(x=sal,y=TA_1500),size=1)+
  #annotate("text", x=31.15, y=2390, size=6,hjust=0,label= "(b) Unknown Order of Mixing") +
  scale_x_continuous(expand = c(0, 0), limits = c(31, 36)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(2000, 2400)) +
  ylab("TA (µmol/kg)")+xlab("Salinity (g/kg)")+
  theme_classic()+theme(text = element_text(size=20))

#combine figures into single panel and export
(a_plot+b_plot)/(c_plot+d_plot) 
ggsave("Figure1.pdf",width = 12, height = 8, units = "in") #post processing edits made to final figure

##############Estimate Freshwater Endmembers##############
# Navigate to folder with Liujendijkt 2020 groundwater discharge and import data
setwd("Liujendijkt_et_al_2020_Supplementary Data")
gw_discharge=read.dbf("coastal_gw_discharge.dbf")
reef_gw_discharge=subset(gw_discharge,coral_reef==1 & fsgds_best!=-99999)
length(reef_gw_discharge$fsgds_best)
setwd('..')

#import river alkalinity data and subset for tropical rivers at latitude<30
Cai_2008_River_Alkalinity <- read_csv("Cai_2008_River_Alkalinity.csv")
cai_tropics = subset(Cai_2008_River_Alkalinity,Lat<30)

#plot fraction of reef groundwater discharge
fgw_plot=
  ggplot()+
  geom_density(data=reef_gw_discharge,aes(x=fsgds_best),alpha=.2, fill="#c51b8a")+
  ylab("Density")+
  xlab("Fresh submarine groundwater discharge / Surface runoff")+
  #scale_x_continuous(expand = c(0,0), breaks = seq(0,1000,50))+
  #scale_y_continuous(expand = c(0,0), breaks = seq(0,0.02,0.005))+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

gw = runif(1,95,13000) #random selection from range of coral reef groundwater alkalinity in Cyronak et al 2013
river = runif(1,224,3115) #random selection from range of global river alkalinity in Cai et al 2008
precip = runif(1,-2.7,18) #random selection from range of rainwater in Avila and Roda 2002 == assuming surface water TA = precipitation TA

#run Monte Carlo simulation of estimate freshwater TA endmembers for the river dominated, precipitation dominated, and surface runoff dominated systems
results=NULL
for (k in 1:10000)
{
  gw_f = sample(reef_gw_discharge$fsgds_best,size=1) #random selection of fsgds_best from Liujendijt et al 2020
  sw_f = runif(1,0,1) #random selection of proportion of river water vs precipitation <- what is a realistic value here?
  TAgw = runif(1,95,13000) #range of coral reef groundwater alkalinity in Cyronak et al 2013
  TAr = runif(1,min(cai_tropics$HCO3),max(cai_tropics$HCO3)) #range of tropical river alkalinity in Cai et al 2008
  TAp = runif(1,-2.7,18) #range of rainwater in Avila and Roda 2002 == assuming surface water TA = precipitation TA
  TAsw = sw_f*TAp+(1-sw_f)*TAr #potential freshwater endmember assuming mixture of river water and precipitation
  em_r = (gw_f*TAgw+TAr)/(gw_f+1)
  em_p = (gw_f*TAgw+TAp)/(gw_f+1)
  em_sw = (gw_f*TAgw+TAsw)/(gw_f+1)
  results=rbind(results,data.frame(gw_f,sw_f,gw,em_r,em_p,em_sw))
}

#quantify probabilities of the respective TA endmember simulations
River_quantiles <- data.frame(x = "River",
                              y0 = quantile(results$em_r, 0.025),
                              y25 = quantile(results$em_r, 0.25),
                              y50 = quantile(results$em_r, 0.5),
                              y75 = quantile(results$em_r, 0.75),
                              y100 = quantile(results$em_r, 0.975))
Precipitation_quantiles <- data.frame(x = "Precipitation",
                                      y0 = quantile(results$em_p, 0.025),
                                      y25 = quantile(results$em_p, 0.25),
                                      y50 = quantile(results$em_p, 0.5),
                                      y75 = quantile(results$em_p, 0.75),
                                      y100 = quantile(results$em_p, 0.975))
Runoff_quantiles <- data.frame(x = "Runoff",
                               y0 = quantile(results$em_sw, 0.025),
                               y25 = quantile(results$em_sw, 0.25),
                               y50 = quantile(results$em_sw, 0.5),
                               y75 = quantile(results$em_sw, 0.75),
                               y100 = quantile(results$em_sw, 0.975))
em_quantiles=bind_rows(River_quantiles,Precipitation_quantiles,Runoff_quantiles)

#plot map of coral reef fractin of submarine groundwater discharge data
global_fsgds_plot=
  ggplot() +
  annotate("text", x=-165, y=78, label= "(a)",size=7)+
  borders("world",fill="#fff7bc")+
  geom_point(data=subset(reef_gw_discharge,fsgds_best>0),aes(x=ws_cent_x,y=ws_cent_y),color="black",size=1,alpha=0.6)+
  scale_x_continuous(expand = c(0,0), breaks = seq(-180,180,30))+
  scale_y_continuous(expand = c(0,0), breaks = seq(-90,90,30))+ 
  ylab("Latitude")+
  xlab("Longitude")+
  coord_equal(ratio=1)+
  #scale_colour_stepsn(colours = c("#225ea8","#41b6c4","#c7e9b4"),breaks=c(0.1,1,100))+
  labs(colour = "FSGDS")+
  theme_classic()+
  theme(text = element_text(size=16),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        #axis.ticks = element_blank(),
        plot.margin=unit(c(-0.30,0,0,0), "null"))

#boxplots of simulated coral reef freshwater TA
endmember_boxplot=
  ggplot(em_quantiles, aes(x,fill=x)) +
  geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),stat = "identity",size=1)+
  annotate("text", x=0.5, y=3150, size=6,label= "(b)") +
  xlab("Endmember Simulation")+
  ylab("Estimated TA @ S=0 (µM)")+
  scale_y_continuous(expand = c(0,0), limits = c(-250,3250),breaks = seq(0,4000,500))+
  scale_colour_manual(name="",values=c("River","rain","surface")) +
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank())
#xport figures and combine them in post-processing
global_fsgds_plot
ggsave("Figure2a.pdf",width = 20, height = 20, units = "cm")
endmember_boxplot
ggsave("Figure2b.pdf",width = 10, height = 10, units = "cm")

##############Cyronak et al 2018 nTA##############
# Navigate to folder with Cyronak 2018 coral reef data
setwd("Cyronak2018")

# Import all Cyronak 2018 data into single data frame
cyronak2018=
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source")

# Step back to parent directory
setwd('..')

# Convert source to number ID tag for simplicity
cyronak2018$source=substr(cyronak2018$source,1,2)

# Rename columns for brevity
colnames(cyronak2018)=c("source","temp","sal","ta","dic")

#remove missing ta and salinity data via -9 values
cyronak2018reef=subset(cyronak2018,ta!=-9&sal!=-9)

#import open ocean DIC + TA data
OOceanTADIC = read_csv("OOceanTADIC.csv", col_names = FALSE)
oceanTADIC = cbind(unique(cyronak2018$source),OOceanTADIC)
colnames(oceanTADIC)=c("source","dic_ocean","ta_ocean")
cyronak2018=merge(cyronak2018reef,oceanTADIC[,c(1:3)],by="source")

#Source 02 and 10 do not report coupled TA and S data so they are removed from the analysis here
cyronak2018=subset(cyronak2018,source!="02"&source!="10")

#add a mean salinity value for each source to use in normalization
cyronak2018_sal=cyronak2018 %>%
  group_by(source) %>%
  mutate(sal_ocean=max(sal),mean_sal=mean(sal))

#add a mean salinity value for each source to use in normalization
cyronak2018_sal2=cyronak2018_sal %>%
  group_by(source) %>%
  mutate(range_sal=max(sal)-(sal))

#mean sal ranges for each reef
cyronak2018_sal_ranges=cyronak2018_sal2 %>%
  group_by(source) %>%
  summarize(mean_range_sal=mean(range_sal))

#summary stats of salinity ranges for all sites
with(cyronak2018_sal_ranges,mean(mean_range_sal))
with(cyronak2018_sal_ranges,sd(mean_range_sal))
with(cyronak2018_sal_ranges,max(mean_range_sal))
with(cyronak2018_sal_ranges,min(mean_range_sal))

#sort salinity data to estimate precision from the reported significant figures
cyronak2018_sal3=cyronak2018_sal2 %>%
  group_by(source) %>%
  mutate(unique_sal=sort(sal,decreasing = FALSE)-lag(sort(sal,decreasing = FALSE)))

#estimate reported precision from salinity data
cyronak2018_sal4=cyronak2018_sal3 %>%
  group_by(source) %>%
  summarize(sal_precision=min(unique_sal[unique_sal>0],na.rm=TRUE))

#estimated salinity precision for each source
mean_S_precision=cyronak2018_sal4 %>%
  group_by(source) %>%
  summarize(mean_precision=mean(sal_precision,na.rm=TRUE))

#calculate nTA using TA=0 at Sal=0 endmember 
cyronak2018nta=cyronak2018_sal3 %>%
  group_by(source) %>%
  mutate(nta_0=(ta/sal)*mean_sal,nta_1000=((ta-1000)/sal)*mean_sal+1000,nta_2000=((ta-2000)/sal)*mean_sal+2000,nta_3000=((ta-3000)/sal)*mean_sal+3000,nta_4000=((ta-4000)/sal)*mean_sal+4000,nta_5000=((ta-5000)/sal)*mean_sal+5000,nta_6000=((ta-6000)/sal)*mean_sal+6000)

#construct linear models (Type II OLS) of TA vs. salinity for each reef and return summaries of the TA vs. salinity models with ±95% confidence intervals
ta_s_summaries= 
  cyronak2018 %>%
  group_by(source) %>%
  do(broom::tidy(lmodel2(ta ~ sal, data = ., range.y=NULL, range.x=NULL, nperm=99))) %>%
  subset(method=="OLS")

#Extract slopes and intercepts from TA vs. Salinity models
ta_s_slopes=subset(ta_s_summaries,term=="Slope")
write_csv(ta_s_slopes,'TableS1.csv')
ta_s_intercepts=subset(ta_s_summaries,term=="Intercept")

#Add intercepts column to Cyronak 2018 data to normalize TA
cyronak_estimates=merge(cyronak2018nta,ta_s_intercepts[,c(1,3)],by="source")

#Compute changes in salinity and TA in normalization process for TA=0 or TA=intercept at Salinity = 0
cyronak2018nta$dsal=with(cyronak2018nta,sal-mean_sal)
cyronak2018nta$dta_0=with(cyronak2018nta,ta-nta_0)

#summary stats of dta_0 for all data sources
with(cyronak2018nta,mean(dta_0))
with(cyronak2018nta,sd(dta_0))
with(cyronak2018nta,max(dta_0))
with(cyronak2018nta,min(dta_0))

#plot TA vs S slopes for each site
ta_s_plot=ggplot(ta_s_slopes, aes(x=source, y=estimate)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "gray") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.3, size=0.5) +
  geom_point(size=3,colour = "black", fill = "white") +
  annotate("text", x=1.5, y=276.9231, label= "(a)",size=7)+
  scale_x_discrete(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-300, 300,100),limits = c(-300, 300)) +
  ylab(bquote('Slope'[' TA vs. Salinity']))+
  theme_classic()+theme(text = element_text(size=20),axis.title.x=element_blank())

#plot range of salinity data for each site
cyronak_sal_plot=
  ggplot(cyronak2018_sal2, aes(x=source, y=range_sal)) +
  geom_jitter(size=2,height=0,width=0.1,alpha=0.2)+
  geom_violin(scale="width",alpha=0.1)+
  #geom_point(size=3)+
  #geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1) +
  #geom_hline(yintercept=0, linetype="dashed", color = "gray") +
  scale_x_discrete(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,6,1),limits = c(0, 6)) +
  annotate("text", x=1.5, y=5.769231, label= "(b)",size=7)+
  ylab("Range of Salinity (g/kg)")+
  ylab(bquote('Salinity'['Range']~'(g/kg)'))+
  theme_classic()+theme(text = element_text(size=20),axis.title.x=element_blank())

#plot change in nTA from TA for data from each site
dTA_source_plot=
  ggplot(cyronak2018nta, aes(x=source, y=dta_0)) + 
  geom_jitter(size=2,height=0,width=0.1,alpha=0.2)+
  geom_violin(scale="width",alpha=0.1)+
  #geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1) +
  #geom_hline(yintercept=0, linetype="dashed", color = "gray") +
  annotate("text", x=1.5, y=132.6923, label= "(c)",size=7)+
  scale_x_discrete(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-350,150,50),limits = c(-350, 150)) +
  ylab(bquote('TA - nTA'['EM=0']~'(µmol/kg)'))+
  theme_classic()+theme(text = element_text(size=20),axis.title.x=element_blank())

#combine figures and export
ta_s_plot/cyronak_sal_plot/dTA_source_plot
ggsave("Figure3.pdf",width = 30, height = 24, units = "cm")

#remove missing TA values from analyses before calculating ∆nTA
cyronak2018_sal=cyronak2018_sal[cyronak2018_sal$ta_ocean!=-9,]
#calculate ∆nTA with 0, 1000, 2000, 3000 freshwater endmembers using S_ocean as reference salinity
cyronak2018_sal$dnTA_ocean = with(cyronak2018_sal,ta_ocean - ta*(sal_ocean/sal))
cyronak2018_sal$dnTA_ocean_1000=with(cyronak2018_sal,ta_ocean-((ta-1000)*(sal_ocean/sal)+1000))
cyronak2018_sal$dnTA_ocean_2000=with(cyronak2018_sal,ta_ocean-((ta-2000)*(sal_ocean/sal)+2000))
cyronak2018_sal$dnTA_ocean_3000=with(cyronak2018_sal,ta_ocean-((ta-3000)*(sal_ocean/sal)+3000))
#calculate ∆nTA with 0 freshwater endmember using mean(S_reef) as reference salinity
cyronak2018_sal$dnTA_reef = with(cyronak2018_sal,ta_ocean*(mean_sal/sal_ocean) - ta*(mean_sal/sal))
#calculate ∆S between Smax and Sreef
cyronak2018_sal$ddsal = cyronak2018_sal$sal_ocean-cyronak2018_sal$sal

#establish colorbars for various endmembers
cols <- c("0 "="#69b3a2","1000 "="#a6bddb","2000 "="#74a9cf","3000 "="#3690c0","4000 "="#0570b0","5000 "="#045a8d","6000 "="#023858")

#calculate mean ∆S and ∆nTA for each reef site
cyronak_means=cyronak2018_sal %>%
  group_by(source) %>%
  summarize(mean_ddsal=mean(ddsal),
            mean_ddnTA_percent=mean(((dnTA_ocean-dnTA_reef)/dnTA_ocean)*100), #calculate ∆nTA percent between ∆nTAocean and ∆nTA reef
            mean_dnTA_ocean=mean(dnTA_ocean)-mean(dnTA_ocean), #calculate ∆nTA=0@S=0 - ∆nTA=0@S=0
            mean_dnTA_1000=mean(dnTA_ocean_1000)-mean(dnTA_ocean), #calculate ∆nTA=0@S=1000 - ∆nTA=0@S=0
            mean_dnTA_2000=mean(dnTA_ocean_2000)-mean(dnTA_ocean), #calculate ∆nTA=0@S=2000 - ∆nTA=0@S=0
            mean_dnTA_3000=mean(dnTA_ocean_3000)-mean(dnTA_ocean)) #calculate ∆nTA=0@S=3000 - ∆nTA=0@S=0

#summaries of type II OLS linear models of uncertainties scaling with salinities
lmodel2(cyronak_means$mean_ddnTA_percent~cyronak_means$mean_ddsal,range.y=NULL,range.x=NULL,nperm=99)
lmodel2(cyronak_means$mean_dnTA_ocean~cyronak_means$mean_ddsal,range.y=NULL,range.x=NULL,nperm=99)
lmodel2(cyronak_means$mean_dnTA_1000~cyronak_means$mean_ddsal,range.y=NULL,range.x=NULL,nperm=99)
lmodel2(cyronak_means$mean_dnTA_2000~cyronak_means$mean_ddsal,range.y=NULL,range.x=NULL,nperm=99)
lmodel2(cyronak_means$mean_dnTA_3000~cyronak_means$mean_ddsal,range.y=NULL,range.x=NULL,nperm=99)

#plot ∆nTA vs ∆S based on various endmembers
mean_em_unc=ggplot()+
  geom_vline(xintercept=1,size=1,linetype='dashed',alpha=0.5)+
  geom_point(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_ocean,colour="0 "),shape=15,size=3,alpha=0.8)+
  geom_smooth(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_ocean, colour="0 "),method=lm , se=TRUE)+
  geom_point(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_1000,colour="1000 "),shape=16,size=3,alpha=0.8)+
  geom_smooth(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_1000, colour="1000 "),method=lm , se=TRUE)+
  geom_point(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_2000,colour="2000 "),shape=17,size=3,alpha=0.8)+
  geom_smooth(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_2000, colour="2000 "),method=lm , se=TRUE)+
  geom_point(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_3000,colour="3000 "),shape=18,size=3,alpha=0.8)+
  geom_smooth(data=cyronak_means,aes(x=mean_ddsal,y=mean_dnTA_3000, colour="3000 "),method=lm , se=TRUE)+
  annotate("text", x=0.15, y=240, size=6,label= "(a)") +
  scale_colour_manual(name="TA@S=0\n(µmol/kg)",values=cols) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0,100,0.2), limits = c(0, 2.25)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,250,50),limits = c(-10, 225)) +
  ylab(bquote('∆nTA'['TA>0@S=0']~'- ∆nTA'['TA=0@S=0']~'(µmol/kg)'))+
  xlab(bquote('S'['max']~'- S'['reef']~'(g/kg)'))+
  theme_classic()+theme(text = element_text(size=20))

#plot percent ∆nTA vs ∆S based on order of mixing
mean_mix_unc=ggplot()+
  geom_vline(xintercept=1,size=1,linetype='dashed',alpha=0.5)+
  geom_point(data=cyronak_means,aes(x=mean_ddsal,y=mean_ddnTA_percent),colour="#023858",size=3,alpha=0.2)+
  geom_smooth(data=cyronak_means,aes(x=mean_ddsal,y=mean_ddnTA_percent),colour="#023858",method=lm , se=TRUE)+
  annotate("text", x=0.15, y=9.6, size=6,label= "(b)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0,100,0.2), limits = c(0, 2.25)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(-1,100,1),limits = c(-0.4, 7)) +
  #ylab(bquote('((∆nTA'['S'['max']]~'- ∆nTA'['S'['mean']]~')/∆nTA'['S'['max']]~')*100'))+
  ylab(bquote('Order of Mixing % ∆nTA'['uncertainty']))+
  xlab(bquote('S'['max']~'- S'['reef']~'(g/kg)'))+
  theme_classic()+theme(text = element_text(size=20))

#generate multipanel figure 4 and export
mean_em_unc+mean_mix_unc
ggsave("Figure4.pdf",width = 32, height = 14.5, units = "cm")
