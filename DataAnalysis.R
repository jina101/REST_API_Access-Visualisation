#### Data Analysis and Visualisation for the Various Data Extracted from GitHub API ####
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(dygraphs)
library(xts)



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

#####################################################
#Visualisation: Total Commits By Month for Each Repo
#####################################################

head(commit.DF)
summary.data.frame(commit.DF)


#1: Group Data by Repository and Month
 commit.DF2 <- commit.DF %>%
  mutate(month = month(CommitDate)) %>%
  group_by(repo_names, month)

 #Get the totals for each group
 commit.DF3 <- as.data.frame(count(commit.DF2))
 
 #Exclude row 5:
 commit.DF3 <- commit.DF3[-5,]
 
 #Plot
 ggplot(commit.DF3, aes(repo_names, n, fill=repo_names)) +
    geom_bar(stat='identity', width=0.5) +
    theme_bw()+
    theme(legend.title =element_text("Repositories"),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
    ylab("Total Commits") +
    xlab("Repositories")
 
 # Create an animated plot
 a <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(0,0,0,0,0), frame=rep('a',5))
 b <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,0,0,0), frame=rep('b',5))
 c <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,2,0,0), frame=rep('c',5))
 d <- data.frame(Repository=c("LCA-Java","LCA-Python","MLA-Assignment", "REST_API_Access-Visualisation", "SoftwareApps-PHP-MySQL"), Commits=c(19,9,20,3,60), frame=rep('d',5))
 
 monthly_data <- rbind(a,b,c,d)
 
p<- ggplot(monthly_data, aes(x=Repository, y=Commits, fill=Repository)) + 
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

animate(p)
anim_save("output.gif")
 
 anim_save("CommitsPerRepo.gif")
 
##################################################################
# Most Popular Commit Times
################################################################## 

 #Create a DateTime column and convert to POSIXct/POSIXt
commit.DF$DateTime <- paste(commit.DF$CommitDate, commit.DF$CommitTime)
commit.DF$DateTime <- dmy_hms(commit.DF$DateTime)

# Make sure it's converted to POSIXct/POSIXt
class(commit.DF$DateTime)

# Group by day
commit.DF7 <- commit.DF %>%
  group_by(day(DateTime))

commit.DF8 <- as.data.frame(count(commit.DF7))

#Group by month
commit.DF10 <- commit.DF %>%
  group_by(month(DateTime))

commit.DF11 <- as.data.frame(count(commit.DF10))

#Group by date
commit.DF14 <- commit.DF %>%
  group_by(date(DateTime))

commit.DF15 <- as.data.frame(count(commit.DF14))


## Create an interactive time series plot

#Create xts
don <- xts(x = commit.DF15$n, order.by = commit.DF15$`date(DateTime)`)

#Commits by Day
t <- dygraph(don,  main = "Commits by Day Oct 2020 - Dec 2020", )%>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

# save the widget
library(htmlwidgets)
saveWidget(t,"CommitsByDayDygraph.html")

#Group by time
commit.DF19$Time <- format(floor_date(commit.DF19$DateTime, "hour"),"%H:%M:%S")
commit.DF19 <- commit.DF19 %>%
  group_by(Time)

group_by_time <- as.data.frame(count(commit.DF19,repo_names, Time))
group_by_time$Time <- as.numeric(substr(group_by_time$Time, start=1, stop = 2))

class(group_by_time$RepoID)

#Save animated plot
anim <- group_by_time %>%
  ggplot( aes(x=Time, y=n, group=as.factor(repo_names), color=repo_names)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Commits by Time Per Time (24hr)") +
  theme_ipsum() +
  ylab("Total Commits") +
  xlab("Time - 24hrs")+
  transition_reveal(Time)

#animate(anim)
anim_save("CommitsPerTime.gif")


?count
commits_by_hour <- count(commit.DF19)
plot(commits_by_hour)

df %>%
  group_by(Date=floor_date(Date, "1 hour")) %>%
  summarize(c1=sum(c1), c2=sum(c2), c3=sum(c3))

commit.DF20 <- as.data.frame(count (floor_date(commit.DF19$DateTime, "hour")))

commit.DF$Hour = commit.DF19$DateTime$hour

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

# Cut into 60 minute intervals
count(commit.DF$CommitSha, commit.DF$RepoID, commit.DF$DateTime)
commit.DF$Interval <- cut(hour(commit.DF$DateTime), c(0, 6, 12, 18, 24), include.lowest = TRUE)
group.by(commit.DF$Interval)
plot(commit.DF$CommitTime)
#commit.DF$interval <- cut(commit.DF$DateTime, breaks = "60 min")  
#commit.DF <- group_by(commit.DF$interval)
 
library(gapminder)
gapminder 
 