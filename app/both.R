library(forecast)
library(RCurl)
library(tidyverse)
library(anomalize)
library(BreakoutDetection)
library(pracma)


##### read data and fix
x1 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data1 <- read.csv(text = x1)
data1$Country.Region<-as.character(data1$Country.Region)
data1$Province.State<-as.character(data1$Province.State)

#### list for dropdown
countrylist<-sort(as.character(unique(data1$Country.Region)))

#### begin anomaly function
anombreak.covid.mort<-function(alpha=0.05, max_anoms=0.2, country="US", n.break=7){
  
  #### create summary stats by day
  out<-data1 %>%
    as.tbl() %>%
    group_by(Country.Region) %>%
    summarise_at(names(data1[,c(5:ncol(data1))]), sum, na.rm=T)  
  
  #### subselector - make this a function with shiny
  plotme<-out[out$Country.Region==country,]
  
  #### create the lag, make actual date column, and organize dataset
  plotme2<-data.frame(diff(t(plotme[,-1])))
  plotme2$ds<-gsub("X", "", row.names(plotme2))
  plotme2$ds<-gsub("\\.", "-", plotme2$ds)
  plotme2$ds<-as.Date(plotme2$ds, format="%m-%d-%y")
  plotme2<-plotme2[,c(2,1)]
  names(plotme2)<-c("ds", "y")
  row.names(plotme2)<-seq(1,nrow(plotme2))
  plotme2$y<-ifelse(plotme2$y<0,0, plotme2$y)
  
  #### total for plot text
  total<-sum(plotme2$y)
  
  #### select from first positive case to currently avaiable date
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
    scale_y_continuous(expand = c(0.05, 0.05) ) +
    ylab("Number of Deaths") +
    coord_cartesian(ylim=c(0, (upper + (10 - upper %% 10)))) +
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
  enddate<-c(startdate[2:length(startdate)], max(plotme4$timestamp))
  
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
  

