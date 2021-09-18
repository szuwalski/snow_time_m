#==numbers at length visualizations
# Bring in observed survey numbers
library(ggplot2)
library(dplyr)
library(reshape)
library(ggridges)
library(gghighlight)
library(gridExtra)

library(gapminder)
library(gganimate)
library(transformr)

# plot(snowad.rep[[1]]$"estimated number of recruits female"[5:38]/1000000~
#        snowad.rep[[1]]$"Predicted Female survey mature Biomass"[1:34],
#      las=1,pch=16,col='grey',ylab='Recruits (1000000)',
#      xlab='Mature biomass (1000t)',xlim=c(0,240),ylim=c(0,2.6))

DATfile <-readLines("20_sq/2016sc.DAT")
surv_yr<-38
tmp<-grep("males new shell immature survey",DATfile)
SurvImmMalNew<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+3):(tmp+surv_yr+2)],split=" "))),nrow=surv_yr,byrow=T)
tmp<-grep("survey males new shell mature",DATfile)
SurvMatMalNew<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+3):(tmp+surv_yr+2)],split=" "))),nrow=surv_yr,byrow=T)
tmp<-grep("survey males old shell mature",DATfile)
SurvMatMalOld<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+2):(tmp+surv_yr+1)],split=" "))),nrow=surv_yr,byrow=T)

tmp<-grep("survey males old shell immature",DATfile)
temp<-unlist(strsplit(DATfile[(tmp+2):(tmp+surv_yr+1)],split=" "))
temp<-unlist(strsplit(temp,split="\t"))
SurvImmMalOld<-matrix(as.numeric(temp),nrow=surv_yr,byrow=T)

totMales<-SurvImmMalNew+SurvMatMalNew+SurvMatMalOld+SurvImmMalOld
colnames(totMales)<-seq(27.5,132.5,5)
rownames(totMales)<-seq(1982,2019)



melted<-melt(totMales)
colnames(melted)<-c("year","Size","value")
melted$country<-"USA"
#==Canada
CA_dat<-read.csv("data/codytrawl.csv")

ca_mut <- CA_dat %>%
  mutate(bin = cut(cw, seq(0, 205 + 5, 5), right = FALSE)) %>%
  filter(sex==1)

ca_mut<-ca_mut %>%
  group_by(year,sex,bin) %>%
  summarise(value = sum(npt))

mid_pts_CA<-seq(2.5,207.5,5)
ca_mut$Size<-mid_pts_CA[match(ca_mut$bin,levels(ca_mut$bin))]
ca_mut$country<-"Canada"
ca_in<-ca_mut[,-c(2,3)]
ca_in<-ca_in[,c(1,3,2,4)]

in_dat<-rbind(ca_in,melted)
in_dat<-filter(in_dat,Size<135 & Size>25)
##==ggridges
xlab <- paste0("\n", xlab)
p <- ggplot(data=in_dat) 
p_natl <- p + geom_density_ridges(aes(x=Size, y=year, height = value, group = year, 
                                 fill=stat(y),alpha=.9999),stat = "identity",scale=3) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0)) +
        #text = element_text(size=20)) +
  labs(x="Carapace width (mm)") +
  facet_wrap(~country)
print(p_natl)

png("plots/n_at_len.png",height=9,width=6,res=400,units='in')
print(p_natl)
dev.off()
