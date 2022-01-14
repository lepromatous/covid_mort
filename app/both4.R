library(forecast)
library(RCurl)
library(tidyverse)
library(anomalize)
library(BreakoutDetection)
library(pracma)

compat<-function(data){
   if(ncol(data)>2){
      return("Error: Uploaded data has too many columns. Please use the template with two columns, date and count or rate.")
   }
   if(nrow(data)<(14*4)){
      return("Error: Data needs at least 56 rows for analysis.")
   }
   
}


###########################################################################################
#### begin anomaly function ####
###########################################################################################
anombreak.covid.mort_upload<-function(data, alpha=0.05, max_anoms=0.2, n.break=7, yaxis="Y axis label", xaxis = "Date", date.labz="%b %Y", breaks = "1 year"){
      

   ###########################################################################################
   #### Date fix/check function ####
   ### make sure the date is correct
   ###########################################################################################
   data[,1]<-as.character(data[,1])
   
   datefix<-function(data){
      if(any(grepl("/", data[,1]))){
         catch<-strsplit(x=data[,1], split="/", fixed=T)
         yr<-lapply(catch, function(x) ifelse(nchar(x[3])==2, paste0("20", x[3]), x[3]))
         mo<-lapply(catch, function(x) ifelse(nchar(x[1])==1, paste0("0", x[1]), x[1]))
         day<-lapply(catch, function(x) ifelse(nchar(x[2])==1, paste0("0", x[2]), x[2]))
         newdate<-as.Date(paste0(yr,"-",mo, "-", day))
      }
      
      if(any(grepl("-", data[,1]))){
         catch<-strsplit(x=data[,1], split="-", fixed=T)
         yr<-lapply(catch, function(x) ifelse(nchar(x[3])==2, paste0("20", x[3]), x[3]))
         mo<-lapply(catch, function(x) ifelse(nchar(x[1])==1, paste0("0", x[1]), x[1]))
         day<-lapply(catch, function(x) ifelse(nchar(x[2])==1, paste0("0", x[2]), x[2]))
         newdate<-as.Date(paste0(yr,"-",mo, "-", day))
      }
      return(newdate)
   }
   
   #### End date fix ####  
       ### fix the date
      data[,1]<-datefix(data)
   
      ### stuff for anomaly
      plotme2<-data
      names(plotme2)<-c("ds", "y")
 
      #### total for plot text
      total<-sum(plotme2$y)

      plotme2 %>%
         mutate(y = pracma::movavg(y, n = (14*4), type="e")) -> plotme2
      
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
                        fill = "#4b0082", alpha=0.5) +
            geom_line(aes(y=a$observed, x=a$ds), color="black", size=0.7) +
            geom_point(aes(y=a$observed, x=a$ds),
                       color=ifelse(a$anomaly=="Yes", "black", "gray35"),
                       fill=ifelse(a$anomaly=="Yes", "red", "gray35"),
                       shape=ifelse(a$anomaly=="Yes", 21, 1),
                       size=ifelse(a$anomaly=="Yes", 3, 0.001)) +
            scale_x_date(expand = c(0.05, 0.05), date_labels = date.labz, date_breaks = breaks) +
            scale_y_continuous(expand = c(0.05, 0.05)) +
            ylab(paste(yaxis)) +
            xlab(paste(xaxis)) +
            coord_cartesian(ylim=c(0, upper2)) +
            theme(axis.text.x = element_text(angle = 90),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.line = element_line(),
                  panel.background = element_blank(),
                  legend.position="bottom") 
        #annotate("text", label = paste0("count", "=", total), x = (min(plotme2$ds)+15), y=max(plotme2$y))
            
            

      #### change data for breakout detector ####
      plotme4<-as_tibble(plotme2)

      names(plotme4)<-c("timestamp", "count")


      ## run breakout algorithm
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
      return(out)
}


