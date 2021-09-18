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
colnames(melted)<-c("Year","Size","value")
##==ggridges
xlab <- paste0("\n", xlab)
p <- ggplot(data=melted) 
p_natl <- p + geom_density_ridges(aes(x=Size, y=Year, height = value, group = Year, 
                                 fill=stat(y),alpha=.9999),stat = "identity",scale=5) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0)) +
        #text = element_text(size=20)) +
  labs(x="Carapace width (mm)")
print(p_natl)

png("plots/n_at_len.png",height=9,width=6,res=400,units='in')
print(p_natl)
dev.off()

dat_2<-rbind(melted,filter(melted,Size>50),filter(melted,Size>75))
dat_2$cutoff<-c(rep(27.5,nrow(melted)),
                rep(50,nrow(filter(melted,Size>50))),
                rep(75,nrow(filter(melted,Size>75))))

p <- ggplot(dat=dat_2) 
p <- p + geom_density_ridges(aes(x=Size, y=Year, height = value, group = Year, 
                                 fill=stat(y),alpha=.9999),stat = "identity",scale=5) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)") +
  facet_wrap(~cutoff,scales='free_x')
png("plots/size_bins_comp.png",height=4.5,width=8,res=400,units='in')
print(p)
dev.off()


#==pull out 2018 numbers at length
natl_2018<-filter(melted,Year==2018)
#==multiply by size transition matrix
size_trans<-as.matrix(read.csv("size_trans.csv",header=F))
#==remove catches
growed<-natl_2018$value %*% size_trans
growed_m<-growed*exp(-0.3)
plot(natl_2018$value)
lines(c(growed))
lines(c(growed_m))
grow_m<-data.frame(Size=seq(27.5,132.5,5),
                   value=c(growed_m))
#==remove natural mortality
#==plot line

png("plots/n_at_len_stack.png",height=8,width=8,res=400,units='in')
stack_p<-ggplot(melted) +
  geom_line(aes(x=Size,y=value,group=Year,col=Year),lwd=1.5) +
  gghighlight(Year > 2014,Year<2019, use_direct_label = FALSE)+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(.7,.7)) +
  labs(y="Numbers (1000000s)",x="Carapace width (mm)") +
  geom_line(data=filter(melted,Year==2019),
            aes(x=Size,y=value),col='red',lwd=1.5) +
  geom_line(data=grow_m,aes(x=Size,y=value),col='green',lwd=1.5,lty=2)
print(stack_p)
dev.off()

png("plots/figure_2.png",height=8,width=4,res=400,units='in')
#grid.arrange(p_natl,stack_p,layout_matrix=matrix(c(1,1,2,2),nrow=2))
grid.arrange(p_natl,stack_p)
dev.off()

png("plots/n_at_len_stack_2014.png",height=8,width=8,res=400,units='in')
p<-ggplot(melted) +
  geom_line(aes(x=Size,y=value,group=Year,col=Year),lwd=1.5) +
  gghighlight(Year > 2009,Year<2015, use_direct_label = FALSE)+
  theme_bw() +
  labs(y="Numbers (1000000s)",x="Carapace width (mm)") +
  geom_line(data=filter(melted,Year==2015),
            aes(x=Size,y=value),col='red',lwd=1.5)
print(p)
dev.off()

p<-ggplot(data=melted)+
  geom_line(aes(x=Size,y=value,group=Year,col=Year))+
  theme_bw()
print(p)


addon<-data.frame(Year=unique(melted$Year),
           Size=25,
           value=0)
melted_2<-rbind(melted,addon)

p<-ggplot(data=melted_2,aes(x=Size,y=value,col=Year,fill=Year)) +
  geom_polygon() +
  theme_bw() +
  scale_fill_viridis_c()+
labs(title = 'Year: {frame_time}') +
  transition_time(Year) +
  ease_aes('cubic-in-out')
anim_save("plots/size_comp.gif",p,end_pause=30,fps=2)

#=======================================
# EBS from the kodiak lab

kod_dat<-read.csv("data/EBSCrab_Abundance_Biomass_m.csv",header=T)
kod_dat_1<-filter(kod_dat,SEX=='MALE')
kod_dat_m<-kod_dat_1 %>%
           group_by(SURVEY_YEAR,SIZE_CLASS_MM) %>%
          summarize(abund=sum(ABUNDANCE))

p <- ggplot(dat=kod_dat_m) 
p <- p + geom_density_ridges(aes(x=SIZE_CLASS_MM, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999),stat = "identity",scale=5) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)") +
  xlim(25,135)
png("plots/size_bins_comp_Kodiak.png",height=9,width=6,res=400,units='in')
print(p)
dev.off()

kod_dat_m_rec<-filter(kod_dat_1,SIZE_CLASS_MM<55 & SEX == 'MALE' & SIZE_CLASS_MM>27.5) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(rec=sum(ABUNDANCE))
plot(data=kod_dat_m_rec,rec~SURVEY_YEAR,type='l')
 
kod_dat_m_rec<-filter(kod_dat_1, SEX == 'MALE' ) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(rec=sum(ABUNDANCE))
plot(data=kod_dat_m_rec,rec~SURVEY_YEAR,type='l')

kod_dat_m_rec<-filter(kod_dat_1,SIZE_CLASS_MM<55 & SEX == 'MALE' & SIZE_CLASS_MM>50) %>%
  group_by(SURVEY_YEAR) %>%
  summarize(rec=sum(ABUNDANCE)/1000000,sd_rec=mean(BIOMASS_MT_CV))
plot(data=kod_dat_m_rec,rec~SURVEY_YEAR,type='l',las=1,xlim=c(1980,2019))
par(new=T)
plot(snowad.rep[[x]]$"estimated number of recruits male"/10000~seq(1982,2019),
     type='l',yaxt='n',ylab='',xlab='',col=2,lty=2,xlim=c(1980,2019))
lines(M[[x]]$recruits[1,]/10000~M[[x]]$mod_yrs,col=x+length(snowad.rep),
      lty=2)

plotr<-data.frame(value=c(scale(kod_dat_m_rec$rec),
           scale(snowad.rep[[x]]$"estimated number of recruits male"),
           scale(M[[x]]$recruits[1,])),
           year=c(seq(1978,2017),seq(1982,2019),seq(1982,2019)),
           mod=c(rep('raw',40),rep('sq',38),rep('gmacs',38)))
p<-ggplot()+
  geom_line(data=plotr,aes(x=year,y=value,group=mod,col=mod),lwd=1.5)+
  theme_bw()+
  ylab(label="Scaled recruitment")
print(p)


#======================================================
# story of the fishery
#======================================================
#==mature male biomass==============
png("plots/fishery_history.png",height=5.5,width=8,res=400,units='in')

par(mfrow=c(3,1),mar=c(.1,.1,.3,.1),oma=c(4,7,3,4))
#plot(-100,xlim=c(1982,2020),ylim=c(0,10),bty='n',xaxt='n',yaxt='n')
#text(x=1991,y=1,"Highest catches")
plot(as.numeric(unlist(snowad.rep[[1]]$"Observed survey male spawning biomass"))[1:SurvYrN[[1]]]~na.omit(SurveyYrs[[1]]),
     pch=16,ylab="Mature Male Biomass (1000 t)",
     xlab="Year",las=1,ylim=c(0,700),
     xaxt='n',bty='n')
#legend("topright",bty='n',"Survey mature male biomass",cex=1.5)
abline(v=1990,lty=2,col='green')
abline(v=1999,lty=2,col='red')
abline(v=2005,lty=2,col='blue')
abline(v=2016,lty=2,col='red')
abline(v=2018,lty=2,col='green')

for(j in 1:length(SurveyYrs[[2]]))
{
  segments(x0=SurveyYrs[[2]][j],x1=na.omit(SurveyYrs[[2]])[j],
           y0=as.numeric(unlist(snowad.rep[[2]]$"Observed survey male spawning biomass"))[j]  /   exp(1.96*sqrt(log(1+snowad.rep[[2]]$"survey CV"[2,]^2)))[j],
           y1=as.numeric(unlist(snowad.rep[[2]]$"Observed survey male spawning biomass"))[j] * exp(1.96*sqrt(log(1+snowad.rep[[2]]$"survey CV"[2,]^2)))[j])
}
mtext(side=2,"Mature biomass",line=3.85)
mtext(side=2,"(1000t)",line=2.5)
par(xpd=NA)
text(side=3,x=1990,y=800,"Highest catches")
text(side=3,x=1999,y=900,"Declared overfished")
text(side=3,x=2005,y=800,"Quotas introduced")
text(side=3,x=2016,y=800,"Lowest MMB")
text(side=3,x=2018,y=900,"Largest recruitment")
par(xpd=TRUE)
#==retained catch biomass

plot(snowad.rep[[2]]$"Observed retained catch biomass"~RetCatchYrs[[2]],las=1,ylab="Retained catch biomass (1000 t)", xaxt="n",ylim=c(0,175),xlim=c(1982,2019),type='l',bty='n')
mtext(side=2,"Retained catch",line=3.85)
mtext(side=2,"(1000t)",line=2.5)
#legend("topright",bty='n',"Retained catch",cex=1.5)
abline(v=1990,lty=2,col='green')
abline(v=1999,lty=2,col='red')
abline(v=2005,lty=2,col='blue')
abline(v=2016,lty=2,col='red')
abline(v=2018,lty=2,col='green')

plot(data=kod_dat_m_rec,rec~SURVEY_YEAR,las=1,
     pch=16,ylim=c(0,1700),xlim=c(1982,2019),bty='n',xaxt='n')
abline(v=1990,lty=2,col='green')
abline(v=1999,lty=2,col='red')
abline(v=2005,lty=2,col='blue')
abline(v=2016,lty=2,col='red')
abline(v=2018,lty=2,col='green')
for(j in 1:length(kod_dat_m_rec$SURVEY_YEAR))
{
  segments(x0=kod_dat_m_rec$SURVEY_YEAR[j],
           x1=kod_dat_m_rec$SURVEY_YEAR[j],
           y0=kod_dat_m_rec$rec[j]  /   exp(1.96*sqrt(log(1+kod_dat_m_rec$sd_rec[j]^2))),
           y1=kod_dat_m_rec$rec[j] * exp(1.96*sqrt(log(1+kod_dat_m_rec$sd_rec[j]^2))))
}
axis(side=1,at=c(1982,1990,2000,2010,2020))
#legend("topleft",bty='n',"Observed recruits (carapace width <55mm)",cex=1.5)
mtext(side=2,"Recruitment",line=3.95)
mtext(side=2,"(billions)",line=2.75)
mtext(side=1,"Year",line=2.5)
dev.off()
