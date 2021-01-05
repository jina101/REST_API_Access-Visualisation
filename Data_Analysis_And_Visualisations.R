#### Data Analysis and Visualisation for the Various Data Extracted from GitHub API ####
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(dygraphs)
library(xts)

######################################################################
#  VISUALISATION 1:

#Interactive Bubble Plot to Visualise Total Repos Vs Total Languages
# of various users/organisations grouped by their continent and with
# point size determined by the number of public members they have
######################################################################

#Read in the data
org.data.DF <- read.csv("fulldata.csv")

#Sort the data by the total number of repos
sorted_data<- org.data.DF[order(org.data.DF[,2]),]


#Create a bubble plot
bubble_plot <- ggplot(sorted_data, aes(x=Repos, y=total_languages, size = public_members, color=continent)) + 
  geom_point(alpha=0.3) +
  scale_size(range = c(.1, 15), name="Public Members")+
  theme_bw() +
  theme(legend.position="right") +
  ggtitle("Orgs/Users: Total Languages vs Total Repositories")+
  ylab("Total Languages") +
  xlab("Total Public Repositories")

#make it interactive
ggplotly(bubble_plot)


######################################################################
#  My Own Github Account Analysis and Visualisation
######################################################################


# Read in Data from CSV file as data frames
repos.DF <- read.csv("repos_data.csv")
commit.DF <- as.data.frame(read.csv("repository_commit_data.csv"))

#Add names to each Repo
repo_names <- c()
for(i in 1: nrow(commit.DF))
{
   if(commit.DF[i,1]== repos.DF[1,1])
   {
      repo_names[length(repo_names)+1] = "LCA-Java"
      
   }else if(commit.DF[i,1]==repos.DF[2,1]){
      
      repo_names[length(repo_names)+1] = "LCA-Python"
      
   }else if(commit.DF[i,1]==repos.DF[3,1]){
   
      repo_names[length(repo_names)+1] = "MLA-Assignment"
      
   }else if(commit.DF[i,1]==repos.DF[4,1]){
      
      repo_names[length(repo_names)+1] = "REST_API_Access-Visualisation"
   }
   else{
      repo_names[length(repo_names)+1] = "SoftwareApps-PHP-MySQL"
   }
}

commit.DF <- cbind(commit.DF, repo_names)

####################################################################################
#Visualisation 2: Animated Bar Plot: Total Commits Per Repo over Oct 2020 - Dec 2020
####################################################################################

head(commit.DF)
summary.data.frame(commit.DF)


#1: Group Data by Repository and Month
 commit_by_repo <- commit.DF %>%
  mutate(month = month(CommitDate)) %>%
  group_by(repo_names, month)

 #Get the totals for each group
 commit_by_repo.totals <- as.data.frame(count(commit_by_repo))
 
 #Exclude row 5 (Jan 2021 Data)
 commit_by_repo.totals <- commit_by_repo.totals[-5,]
 
 #Regular Bar Plot
 ggplot(commit_by_repo.totals, aes(repo_names, n, fill=repo_names)) +
    geom_bar(stat='identity', width=0.5) +
    theme_bw()+
    theme(legend.title =element_text("Repositories"),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
   ggtitle("Total Commits Per Repository Oct 2020 - Dec 2020")+
    ylab("Total Commits") +
    xlab("Repositories")
 
 # Create an animated plot
 a <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(0,0,0,0,0), frame=rep('a',5))
 b <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,0,0,0), frame=rep('b',5))
 c <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,2,0,0), frame=rep('c',5))
 d <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,20,3,60), frame=rep('d',5))
 
 monthly_data <- rbind(a,b,c,d)
 
 
animated_bar<- ggplot(monthly_data, aes(x=Repository, y=Commits, fill=Repository)) + 
    geom_bar(stat='identity') +
    theme_bw() +
   theme(axis.text.x=element_blank(),
         axis.ticks.x=element_blank())+
   ggtitle("Total Commits Per Repository Oct 2020 - Dec 2020")+
    # gganimate specific bits:
    transition_states(
       frame,
       transition_length = 5,
       state_length = 1
    ) +
   shadow_mark(alpha = 0.3, size = 0.5)+
    ease_aes('sine-in-out')

#Save the animated bar plot as a gif
animate(animated_bar)
anim_save("AnimatedBar-CommitsPerRepoOct2020-Nov2020.gif")
 
##################################################################
# Visualisation 3: Dygraph: Commits By Date
################################################################## 

 #Create a DateTime column and convert to POSIXct/POSIXt
commit.DF$DateTime <- paste(commit.DF$CommitDate, commit.DF$CommitTime)
commit.DF$DateTime <- dmy_hms(commit.DF$DateTime)

# Make sure it's converted to POSIXct/POSIXt
class(commit.DF$DateTime)

#Group by date
commit.DF_bydate <- commit.DF %>%
  group_by(date(DateTime))

commit.DF_bydate.total <- as.data.frame(count(commit.DF14))

## Create an interactive time series plot

#Create xts
don <- xts(x = commit.DF_bydate.total$n, order.by = commit.DF_bydate.total$`date(DateTime)`)

#Create interactive the dygraph
#Commits by Day
dygraph1 <- dygraph(don,  main = "Commits by Day Oct 2020 - Dec 2020", )%>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

# save the widget
library(htmlwidgets)
saveWidget(dygraph1,"CommitsByDateDygraph-Interactive.html")


###############################################################################
# Visualisation 4: Animated Line Chart: Commits by Time (24hr) Grouped by Repo
############################################################################### 
#Group by time (hourly)
commit.DF.hour <- commit.DF
commit.DF.hour$Time <- format(floor_date(commit.DF.hour$DateTime, "hour"),"%H:%M:%S")
commit.DF.hour <- commit.DF.hour %>%
  group_by(Time)

#convert time from character to numeric
group_by_time <- as.data.frame(count(commit.DF19,repo_names, Time))
group_by_time$Time <- as.numeric(substr(group_by_time$Time, start=1, stop = 2))

#Create and Save animated plot
anim_line_plot <- group_by_time %>%
  ggplot( aes(x=Time, y=n, group=as.factor(repo_names), color=repo_names)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Commits by Time(24hr)") +
  theme_ipsum() +
  ylab("Total Commits") +
  xlab("Time - 24hrs")+
  transition_reveal(Time)

anim_save("AnimatedLinePlot-CommitsByTime.gif")


################ Upload to Plotly #############

Sys.setenv("plotly_username"="jina101")
Sys.setenv("plotly_api_key"="crSKjKjndZ2gJMrtSP8Y")


api_create(bubble_plot)
api_create(animated_bar)
api_create(dygraph1)
api_create(anim_line_plot)




############################################################
q <- commit.DF %>% 
  count(week = floor_date(commit.DF$DateTime, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

ggplotly(q)

r <- commit.DF %>% 
  count(day = floor_date(commit.DF$DateTime, "day")) %>% 
  ggplot(aes(day, n)) +
  geom_line()

ggplotly(r)

s <- commit.DF %>% 
  count(hour = floor_date(commit.DF$DateTime, "hour")) %>% 
  ggplot(aes(hour, n)) +
  geom_line()

ggplotly(s)


 