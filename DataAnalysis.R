#### Data Analysis and Visualisation for the Various Data Extracted from GitHub API ####
library(dplyr)
library(lubridate)
library(tibbletime)
library(data.table)
library(ggplot2)
library(gganimate)
library(gifski)

# Read in Data from CSV file as data frames
repos.DF <- read.csv("repos_data.csv")
repos.DF <- repos.DF[,-1]
commit.DF <- as.data.frame(read.csv("repository_commit_data.csv"))
commit.DF <- commit.DF[,-1]

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
 
 #Exclude row 4:
 commit.DF3 <- commit.DF3[-5,]
 
 ggplot(commit.DF3, aes(repo_names, n, fill=repo_names)) +
    geom_bar(stat='identity', width=0.5) +
    theme_bw()+
    theme(legend.title =element_text("Repositories"),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())+
    ylab("Total Commits") +
    xlab("Repositories")
 
 # Create an animated plot
 names<-unique(commit.DF3$repo_names)
 
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
   ggtitle("Total Commits Per Repository Oct 2020 - Nov 2020")+
    # gganimate specific bits:
    transition_states(
       frame,
       transition_length = 5,
       state_length = 1
    ) +
   shadow_mark(alpha = 0.3, size = 0.5)+
    ease_aes('sine-in-out')

animate(p) #, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("output.gif")
 
 anim_save("CommitsPerRepo.gif")
 
 #Commits, repos over time, languages over time
 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
# Monthly Commit Data Per Repo
 commit.DF3 <- data.frame(
   
   Repository = integer(),
   October = integer(),
   November = integer(),
   December = integer()
   
   
 )
 
 #Function to count the number of commits per month per repo
 Oct_count = 0
 Nov_count  = 0
 Dec_count = 0
 
 count_commits <- function(dataframe, repo)
 {
   
   if(dataframe[,1] == repo && dataframe[,5] == 10){
     Oct_count = Oct_count + 1
     
   }else if(dataframe[,1] == repo && dataframe[,5] == 11){
     Nov_count  = Nov_count+ 1
     
   } else{
     
     Dec_count = Dec_count + 1
   }
   
   return(Dec_count)
   # insert into dataframe
   
   #commit.DF[nrow(commit.DF)+1,] = c(repo, Oct_count, Nov_count, Dec_count)
 }
 
 
 for(i in 1: nrow(commit.DF) )
 {
   if(commit.DF2$RepoID == 302588136)
   {
     total <- count_commits(commit.DF2, 302588136 )
   }
 }
 
 
 
  




