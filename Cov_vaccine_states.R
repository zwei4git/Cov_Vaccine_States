library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(datasets)
library(ggimage)

#Data source 
cov_vac<-read.csv2('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv
                   ',header = TRUE,sep = ',')
cov_vac<-as.data.frame(cov_vac)
head(cov_vac)
colnames(cov_vac)

#check locations
table(cov_vac$location)

#data prep
cov_vac%>%
  select(date,location,people_vaccinated_per_hundred)%>%
  mutate(people_vaccinated_per_hundred=as.numeric(as.character(people_vaccinated_per_hundred)),
         date=as.Date(date))%>%
  #fill missing values with closest previous value
  arrange(date)%>%
  group_by(location)%>%
  fill(people_vaccinated_per_hundred,.direction = 'down')->cov_vac2  

#remove Jan 16 to Jan 18, and Feb 15 due to missing; only include 50 US states 
#(add in "New York state" as "New York" was dropped out)
cov_vac2%>%
  filter((location %in% state.name | location=='New York State') &
           ('2021-01-16' > date | date > '2021-01-18') & (date != '2021-01-16'))%>%
  group_by(date)%>%
  arrange(date, desc(people_vaccinated_per_hundred)) %>%
  mutate(ranking = row_number())%>%
  filter(ranking <=15)->cov_vac3


cov_vac3%>%
  ggplot()+
  geom_col(aes(x=ranking,y=people_vaccinated_per_hundred,fill=location))+
  geom_text(aes(x=ranking,y=-1.2,label=location,group=location),hjust=1,size=4)+
  geom_text(aes(x=ranking,y=people_vaccinated_per_hundred+1.2,
                label=format(round(people_vaccinated_per_hundred,2),nsmall = 2),
                group=location),hjust=0)+
  geom_image(aes(x=ranking,y=people_vaccinated_per_hundred+0.6,group=location), 
             image = "https://www.pngrepo.com/download/81525/vaccine.png") +
  labs(title='Date: {closest_state}', y = 'People Vaccinated per Hundred Population', x = NULL)+
  coord_flip(clip = 'off')+
  scale_x_reverse()+
  theme_bw()+
  theme(axis.line.x = element_line(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(1,1,1,2.5),units = 'cm'))+
  transition_states(date, transition_length = 10,state_length = 1,wrap = FALSE)+
  ease_aes('cubic-in-out')->vac_anim

animate(vac_anim,duration = 26, fps = 10, end_pause = 20,
        height=500,width=600, res=100,renderer = gifski_renderer())
