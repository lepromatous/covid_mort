library(forecast)
library(RCurl)
library(tidyverse)
library(anomalize)
library(BreakoutDetection)

data3<-read.csv("https://raw.githubusercontent.com/slu-openGIS/MO_HEALTH_Covid_Tracking/master/data/metro/stl_hospital.csv")
data3<-subset(data3, select=-c(vent_avg, new_in_pt_avg, in_pt_avg, icu_avg))

data3$report_date<-as.Date(data3$report_date)
names(data3)<-c("Date", "Incident Inpatient Admissions", "Prevalent Inpatients", "Prevalent ICU Patients", "Prevalence of Patients on Ventilators", "Total Discharges", "Incident Discharges by Day")

#### list for dropdown
ddlist<-c(as.character(sort(names(data3)[-1])))



#### begin anomaly function
anombreak.covid.mort_task<-function(alpha=0.05, subchoice = "Total Discharges", max_anoms=0.2, n.break=7){
      
      #### create summary stats by day
      out1<-as.numeric(data3[,subchoice])
      out2<-data3$Date
      out<-tibble(out2, out1)
      out<-na.omit(out)
      

      ### stuff for anomaly
      plotme2<-out
      names(plotme2)<-c("ds", "y")
 
      #### total for plot text
      total<-sum(plotme2$y)

      # #### select from first positive case to currently avaiable date
      # first<-min(which(plotme2$y != 0))
      # last<-nrow(plotme2)
      # plotme2<-plotme2[first:last,]

      #### anomaly
      plotz<-as_tibble(plotme2)
      x<-time_decompose(data=plotz, target=y,  frequency="auto", trend="auto", method="stl")
      z<-anomalize(data=x, target = remainder, alpha = alpha, max_anoms = max_anoms, method="gesd")
      a<-time_recompose(z)

      ### make upper limit
      upper<-ifelse(max(a$recomposed_l2) > max(plotme2$y),
                    max(a$recomposed_l2), max(plotme2$y))

      #### make the plot look nice
      upper2<-ifelse(max(plotme2$y)>=20, (upper + (5 - upper %% 5)),(upper + (2 - upper %% 2)))


      plotz<-ggplot() +
            geom_ribbon(aes(ymin = a$recomposed_l1, ymax = a$recomposed_l2, x=a$ds),
                        fill = "#003DA5", alpha=0.25) +
            geom_line(aes(y=a$observed, x=a$ds), color="gray35", size=0.7) +
            geom_point(aes(y=a$observed, x=a$ds),
                       color=ifelse(a$anomaly=="Yes", "black", "gray35"),
                       fill=ifelse(a$anomaly=="Yes", "red", "gray35"),
                       shape=ifelse(a$anomaly=="Yes", 21, 1),
                       size=ifelse(a$anomaly=="Yes", 3, 0.001)) +
            scale_x_date(date_breaks="1 week", expand = c(0.05, 0.05), name="") +
            scale_y_continuous(expand = c(0.05, 0.05)) +
            ylab(subchoice) +
            coord_cartesian(ylim=c(0, upper2)) +
            theme(axis.text.x = element_text(angle = 90),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_line(color="lightgray"),
                  axis.line = element_line(),
                  panel.background = element_blank(),
                  legend.position="bottom") +
            if(subchoice == "Total Discharges" || subchoice == "Prevalent Inpatients"){
                  annotate("text", label = paste0(subchoice, "=", total), x = (min(plotme2$ds)+5.75), y=max(plotme2$y))
            } else{ annotate("text", label=paste0("Total ", subchoice, "=", total), x=(min(plotme2$ds)+25), y=max(plotme2$y))}
            
            


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
                         linetype="dashed",
                         color="#654F97",
                         lwd=0.9,
                         show.legend = T)
      return(plotz2)
      return(out)
}

