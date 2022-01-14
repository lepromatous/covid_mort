library(forecast)
library(RCurl)
library(tidyverse)
library(anomalize)
library(BreakoutDetection)
library(pracma)
##### read data and fix
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
data <- read.csv(text = x)

data$date<-as.Date(data$date)
data$state<-as.character(data$state)
data$county<-as.character(data$county)

#### list for dropdown
statelist<-c("All", sort(as.character(unique(data$state))))
countylist<-c("All", sort(as.character(data$county)))

#### begin anomaly function
anombreak.covid.mort_sc<-function(alpha=0.05, max_anoms=0.2, state="All", county="All", n.break=7){
  
  #### create summary stats by day
  out<-data %>%
    as.tbl() %>%
    group_by(state, county) %>%
    mutate(day.death = deaths- lag(deaths)) %>%
    mutate(day.cases = cases - lag(cases))


#### fix issues with data
out$day.death<-ifelse(out$day.death<0,0,out$day.death) 

##### subsetter
if(state=="All"){
  out<-out %>%
      as.tbl()%>%
      group_by(date) %>%
      summarise(day.death.sum=sum(day.death, na.rm=T))
} else if(state!="All" & county=="All"){
  out<-out[out$state==state,]
  out<-out %>%
    as.tbl()%>%
    group_by(date) %>%
    summarise(day.death.sum=sum(day.death, na.rm=T))
} else if(state!="All" & county!="All"){
  out<-out[out$state==state & out$county==county,]
  out<-out %>%
    as.tbl()%>%
    group_by(date) %>%
    summarise(day.death.sum=sum(day.death, na.rm=T))
}

### stuff for anomaly
 plotme2<-out
  names(plotme2)<-c("ds", "y")
 
  #### total for plot text
  total<-sum(plotme2$y)
  
  # #### select from first positive case to currently avaiable date
  # first<-min(which(plotme2$y != 0))
  # last<-nrow(plotme2)
  # plotme2<-plotme2[first:last,]
  plotme2 %>%
    mutate(y = pracma::movavg(y, n = (14*4), type="e")) -> plotme2
  
  #### anomaly
  plotz<-as_tibble(plotme2)
  x<-time_decompose(data=plotz, target=y,  frequency="auto", trend="auto", method="stl")
  z<-anomalize(data=x, target =remainder, alpha = alpha, max_anoms = max_anoms, method="gesd")
  a<-time_recompose(z)

  ### make upper limit
  upper<-ifelse(max(a$recomposed_l2) > max(plotme2$y),
                max(a$recomposed_l2), max(plotme2$y))
  
  #### make the plot look nice

  upper2<-ifelse(max(plotme2$y)>=20, (upper + (5 - upper %% 5)),(upper + (2 - upper %% 2)))
               

plotz<-ggplot() +
  geom_ribbon(aes(ymin = a$recomposed_l1, ymax = a$recomposed_l2, x=a$ds),
                       fill = "#4b0082", alpha=0.5) +
  geom_line(aes(y=a$observed, x=a$ds), color="black", size=0.7) +
  geom_point(aes(y=a$observed, x=a$ds), 
              color=ifelse(a$anomaly=="Yes", "black", "gray35"), 
              fill=ifelse(a$anomaly=="Yes", "red", "gray35"), 
              shape=ifelse(a$anomaly=="Yes", 21, 1), 
              size=ifelse(a$anomaly=="Yes", 3, 0.001)) +
    scale_x_date(date_breaks="1 week", expand = c(0.05, 0.05), name="") +
    scale_y_continuous(expand = c(0.05, 0.05)) +
    ylab("Number of Deaths") +
    coord_cartesian(ylim=c(0, upper2)) +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(),
          panel.background = element_blank(),
          legend.position="bottom") +
    annotate("text", label=paste0("Total deaths = ", format(total, digits=0, scientific=F, big.mark=",")), x=(min(plotme2$ds)+20), y=max(plotme2$y))


  #### Get country list
  #### subselector - make this a function with shiny
  plotme4<-as_tibble(plotme2)
  
  names(plotme4)<-c("timestamp", "count")

  
  ## run breakout algorightm
  res.break = BreakoutDetection::breakout(plotme4, min.size=n.break, method='multi', beta=.001, degree=1, plot=TRUE)
  plotme4$timestamp<-as.Date(plotme4$timestamp)
  
  ## create mean segments
  segs<-c(0, res.break$loc, nrow(plotme4))
  means<-vector("list", length(segs))
  group<-NULL
  for(i in 1:(length(segs)-1)){
    group<-c(group,rep(i, times=diff(segs)[i]))
  }
  plotme4$group<-factor(group)
  groupmean<-tapply(plotme4$count, plotme4$group, function(x) mean(x))
  
  segment_data = data.frame(
    x = segs[2:length(segs)-1],
    xend = segs[2:length(segs)], 
    y = groupmean,
    yend = groupmean
  )
  segment_data$x <- ifelse(segment_data$x==0, 1, segment_data$x)
  startdate<-as.Date(unlist(lapply(segment_data$x, function(x) plotme4$timestamp[x])), origin="1970-01-01")
  
  if(length(startdate)==1){
    enddate<-max(plotme4$timestamp)
  } else {
    enddate<-c(startdate[2:length(startdate)], max(plotme4$timestamp))
  }
 
  plotz2<-plotz + 
    geom_segment(aes(x = startdate, 
                     y = segment_data$y, 
                     xend = enddate, 
                     yend = segment_data$yend), 
                 linetype="dotted",
                 color="#5c5b5b", 
                 lwd=0.9, 
                 show.legend = T)
return(plotz2)
}
  
