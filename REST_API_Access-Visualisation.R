library(ggplot2)
library(plotly)
library(dbplyr)
library(viridis)
library(hrbrthemes)
#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)


#########################################################################################
#                      Access the API and Retrieve some Data 
#########################################################################################

#Our endpoint is github
oauth_endpoints("github")

# Name of App, key and secret 
myapp <- oauth_app(appname = "REST_API_Visualisation",
                   key = "7471d2a0322c6e108c48",
                   secret = "f648b73c315cd4f65ec16f025effd568d9de45c0")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API (accessing CSSEGISandData's Covid-19 Repo forks)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/repos/CSSEGISandData/COVID-19/forks", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF
gitDF[gitDF$full_name == "CSSEGISandData/datasharing"] 
str(gitDF$commits)

#To the above for other repos and explore the data available

##CERN
req2 <- GET("https://api.github.com/users/CERN/repos", gtoken)
json1 = content(req2)
gitDFCERN = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDFCERN[gitDFCERN$full_name == "CERN/datasharing"] 
str(gitDFCERN)
gitDFCERN[7]

##sindresorhus
req3 <- GET("https://api.github.com/users/sindresorhus/repos", gtoken)
json1 = content(req3)
gitDFsindresorhus = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDFsindresorhus[gitDFsindresorhus$full_name == "sindresorhus/datasharing"] 
gitDFsindresorhus[7]

#hadley
hadley_orgs <- fromJSON("https://api.github.com/users/hadley/orgs")
hadley_repos <- fromJSON("https://api.github.com/users/hadley/repos")
gg_commits <- fromJSON("https://api.github.com/repos/hadley/ggplot2/commits")
gg_commits
hadley_repos

gg_issues <- fromJSON("https://api.github.com/repos/hadley/ggplot2/issues")

paste(format(gg_issues$user$login), ":", gg_issues$title)
paste(format(gg_commits$author$login), ":", gg_commits$commit$message)

###############################################################################################
#                          Retrieve Data for Visualisation Purposes               
###############################################################################################
# Visualisation 1: Get Data for Total Languages vs Total Repos for 100 Github Users/Orgs

# Get GitHub data for 100 Organisations
organisations <- GET("https://api.github.com/organizations?per_page=100")
orgs = content(organisations)
organisations.DF = jsonlite::fromJSON(jsonlite::toJSON(orgs))

#Check data retrieved
organisations.DF
dim(organisations.DF)

# Store Organsation Names
org_user <- organisations.DF$login
org_user_list <- c(org_user)

# Data frame to store relevant organisation data
org.data.DF <- data.frame(
  
  OrgName = integer(),
  Repos = integer(),
  Location = integer()
  
)

# add all the organisations and their info to the dataframe using a for loop
total_orgs <- length(org_user_list)

for(i in 1:total_orgs )
{
  # Get the organisation from the organisation list
  organisation_url <- paste("https://api.github.com/users/", org_user_list[i], sep = "")
  org2 = GET(organisation_url, gtoken)
  org_cont = content(org2)
  
  # if there are no languages, do not include the organisation
  if(length(org_cont) == 0)
  {
    next
  } 
  
  org.DF1 <- jsonlite::fromJSON(jsonlite::toJSON(org_cont))
  org_login <- org.DF1$login
  
  
  # get total repos
  total_repos = org.DF1$public_repos
  
  # get location
  location = org.DF1$location
  
  # add data to a new row in dataframe
  org.data.DF[nrow(org.data.DF) + 1, ] = c(org_login, total_repos, location)
  
  
}

#Save csv file
#write.csv(org.data.DF, 'orgdata.csv')
  
## Get Total Public Members for Each Org:
# Vector to store number of public members
public_members = c()

for(i in 1:nrow(org.data.DF))
{
  public_members_url = paste("https://api.github.com/orgs/", org.data.DF[i,1], "/public_members", sep = "")
  pubmem = GET(public_members_url, gtoken)
  pubmem_cont = content(pubmem)
  pubmem.DF = jsonlite::fromJSON(jsonlite::toJSON(pubmem_cont))
  
  total <- 0
  amount <- nrow(pubmem.DF)
  
  #Get total public employees
  if(is.null(amount))
  {
    total = 0
  }
  else
  {
    total = nrow(pubmem.DF)
  }
  
  #Add to public_members
  public_members[length(public_members) + 1] = total
  
}
#write.csv(public_members, 'public_members.csv')

##Get total languages used in each repo for each org

# function to get the total unique languages:

total_lang<- function(name, total_repos)
{
  languages = c()
  for(j in 1: length(total_repos))
  {
    #Go through all of the organisation's repos
    repos_url2 = paste("https://api.github.com/repos/", name,"/", total_repos[j], sep = "")
    repo2 = GET(repos_url2, gtoken)
    repo_cont2 = content(repo2)
    repo.DF2 = jsonlite::fromJSON(jsonlite::toJSON(repo_cont2))
    
    language <- repo.DF2$language
    
    #Don't include repos that have no languages
    if (length(language) != 0 && language != "<NA>")
    {
      # add language to list
      languages[length(languages)+1] = language
    }
    next
  }
  
  #Get the unique number of languages they have
  total <- length(unique(languages))
  return(total)
  
}  

#vector to store total languages
total_languages = c()

#Go through each org and their repos to get total unique languages
for(i in 1:nrow(org.data.DF))
{
  repos_url = paste("https://api.github.com/users/", org.data.DF[i,1], "/repos?per_page=100", sep = "")
  repo = GET(repos_url, gtoken)
  repo_cont = content(repo)
  repo.DF = jsonlite::fromJSON(jsonlite::toJSON(repo_cont))
  
  total_repos = repo.DF$name
  name = org.data.DF[i,1]
  total <- total_lang(name, total_repos)
  
  #store in vector
  total_languages[length(total_languages)+1] = total
}

#Append them to the previous dataframe and update invalid location figures

org.data.DF <- cbind(org.data.DF, total_languages, public_members)

#Update invalid org locations
org3 = GET("https://api.github.com/organizations/", org_user_list[i], sep = "", gtoken)
org_cont2 = content(org3)
org.DF2 <- jsonlite::fromJSON(jsonlite::toJSON(org_cont2))
public_members <- org.DF2$public_members

#
for(i in 1: nrow(org.data.DF))
{
  if(org.data.DF[i,1] == org.data.DF[i,3])
  {
    org.data.DF[i,3] = "No Location"
  }
}


# Add the continents to the datat 
continent <- c("Unknown", "North America", "North America", "North America", "North America", "Unknown", "North America", "North America","North America"," Europe", "North America", "Europe", "North America", "Europe", "South America", "Unknown", "North America", "North America", "Europe", "Europe", "North America", "Unknown", "Europe", "North America","North America", "North America", "North America", "Europe","Unknown","North America", "North America", "Oceania","North America", "Europe", "Unknown", "North America", "North America", "Unknown", "North America", "South America", "Europe", "North America","North America Europe","Europe", "North America", "Europe", "Unknown","North America", "North America", "North America", "Africa", "North America", "Europe", "Europe", "North America", "Europe","Unknown", "North America", "North America", "Africa","North America","Europe","North America", "North America","Unknown","Unknown","Unknown", "North America", "North America","Unknown","Europe", "North America", "North America", "North America", "North America", "North America", "North America", "Asia","Oceania","North America","North America", "Europe","Europe", "North America", "Unknown", "Europe", "North America", "North America", "Europe", "Europe", "North America", "North America", "North America", "North America", "Europe", "Unknown", "Unknown", "Unknown","North America", "Unknown" )
#write.csv(continent,"continent.csv")

org.data.DF <- cbind(org.data.DF, continent)

#Save data as a csv for visualisation later
write.csv(org.data.DF, "fulldata.csv")

############################ Try Some Hierarchical Clustering with the Data ##############


##########################################################################################
###                     Visualisation of My Personal Repositories                      ###
##########################################################################################
# Extract data
jina101 <- GET("https://api.github.com/users/jina101", gtoken)
jina_content = content(jina101)
jina.DF = jsonlite::fromJSON(jsonlite::toJSON(jina_content))

repos.data.DF <- data.frame(
  
  ID = integer(),
  Name = integer(),
  Created = integer(),
  Updated = integer(),
  Commits_URL = integer(),
  Language_URL = integer(),
  Total_Languages = integer(),
  Repo_URL = integer()
  
)

## Fetch Repository Data:
jina_repos <- GET(jina.DF$repos_url, gtoken)
jinarep <- content(jina_repos)
jinarepo.DF <- jsonlite::fromJSON(jsonlite::toJSON(jinarep))

repo_names <- jinarepo.DF$name
for(i in 1:length(repo_names))
{
 
  lang = c()
  url <- paste("https://api.github.com/repos/jina101/", repo_names[i], sep="" )
  rep_info <- GET(url, gtoken)
  rep_content <- content(rep_info)
  rep.DF <- jsonlite::fromJSON(jsonlite::toJSON(rep_content))

  id <- rep.DF$id
  name <- rep.DF$name
  created = substr(rep.DF$created_at, start=1, stop=10)
  updated = substr(rep.DF$updated_at, start=1, stop=10)
  commits = paste(url, "/commits", sep="")
  language = paste(url,"/languages",sep="")
  
  languages <- rep.DF$language
  if (length(languages) != 0 && languages != "<NA>")
  {
    # add language to list
    lang[length(lang)+1] = languages
  }
  total_lang <- length(unique(lang))
    
  rep_url = url
    
  repos.data.DF[nrow(repos.data.DF)+1, ]=c(id, name, created, updated, commits, language, total_lang, rep_url)
}

write.csv(repos.data.DF, "repos_data.csv")


# Fetch Commit Data for each repo
repo.commit.DF <- data.frame(
  
  RepoID = integer(),
  CommitSha = integer(),
  CommitDate = integer(),
  CommitTime = integer()
  
  
)

for(i in 1:length(repo_names))
{
  com_url <- paste("https://api.github.com/repos/jina101/", repo_names[i],"/commits?per_page=100", sep="" )
  com_info <- GET(com_url, gtoken)
  com_content <- content(com_info)
  commits.DF <- jsonlite::fromJSON(jsonlite::toJSON(com_content))
  

  for(j in 1:nrow(commits.DF))
  {
    id = repos.data.DF[i,1]
    currentCommit = commits.DF[j,]
    commit_sha = currentCommit$sha
    commit_date = substr(currentCommit$commit[,1]$date, start=1, stop=10)
    commit_time = substr(currentCommit$commit[,1]$date, start=12, stop=19)
    
    repo.commit.DF[nrow( repo.commit.DF)+1, ]=c(id, commit_sha, commit_date, commit_time)
  }
  
  #write.csv(repo.commit.DF, "repository_commit_data.csv")
  
}



# Animated plot of total repos and total commits over a few months

# Commits by Repository
library("data.table")
commits.DT <- setDT(repo.commit.DF)
repo.commit.DF[,1] = sapply(repo.commit.DF[,1], as.numeric)

summarise(commits.DT)

summary(commits.DT)
# Stars and Forks of CERNS Repos

# Get Stars and Forks of Some Popular Github Repos

repos <- c("CSSEGISandData/COVID-19", "kubernetes/kubernetes", "Microsoft/vscode", "NixOS/nixpkgs", "Automattic/wp-calypso",
           "")

# Visualisation 1: Location of everyone who forked John Hopkin's Covid Data



