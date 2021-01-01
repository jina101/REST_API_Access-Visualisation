library(ggplot2)
#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

#usethis::edit_r_environ()


oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "REST_API_Visualisation",
                   key = "7471d2a0322c6e108c48",
                   secret = "f648b73c315cd4f65ec16f025effd568d9de45c0")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
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

##CERN
req2 <- GET("https://api.github.com/users/CERN/repos", gtoken)
json1 = content(req2)
gitDFCERN = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDFCERN[gitDFCERN$full_name == "CERN/datasharing"] 
str(gitDFCERN)
gitDFCERN[7]
?GET

##sindresorhus
req3 <- GET("https://api.github.com/users/sindresorhus/repos", gtoken)
json1 = content(req3)
gitDFsindresorhus = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDFsindresorhus[gitDFsindresorhus$full_name == "sindresorhus/datasharing"] 
gitDFsindresorhus[7]

hadley_orgs <- fromJSON("https://api.github.com/users/hadley/orgs")
hadley_repos <- fromJSON("https://api.github.com/users/hadley/repos")
gg_commits <- fromJSON("https://api.github.com/repos/hadley/ggplot2/commits")
gg_commits
hadley_repos

gg_issues <- fromJSON("https://api.github.com/repos/hadley/ggplot2/issues")

paste(format(gg_issues$user$login), ":", gg_issues$title)
paste(format(gg_commits$author$login), ":", gg_commits$commit$message)

######################## Visualisation ###############################################################################

library(rlist)
library(pipeR)

#######################################################################################################################
# Visualisation 1: Total Languages vs Total Repos for 100 GitHub Repos

# Get GitHub data for 100 Organisations
organisations <- GET("https://api.github.com/organizations?per_page=100")
orgs = content(organisations)
organisations.DF = jsonlite::fromJSON(jsonlite::toJSON(orgs))

organisations.DF
dim(organisations.DF)

# Store Organsation Names
org_user <- organisations.DF$login
org_user_list <- c(org_user)

# Data frame to store relevant organisation data
org.data <- c()
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


#write.csv(org.data.DF, 'orgdata.csv')
  
## Get Total Public Members for Each Org:

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

## Get total languages used in each repo for each org

# function to get the total unique languages:

total_lang<- function(name, total_repos)
{
  languages = c()
  for(j in 1: length(total_repos))
  {
    repos_url2 = paste("https://api.github.com/repos/", name,"/", total_repos[j], sep = "")
    repo2 = GET(repos_url2, gtoken)
    repo_cont2 = content(repo2)
    repo.DF2 = jsonlite::fromJSON(jsonlite::toJSON(repo_cont2))
    
    language <- repo.DF2$language
    if (length(language) != 0 && language != "<NA>")
    {
      # add language to list
      languages[length(languages)+1] = language
    }
    next
  }
  
  total <- length(unique(languages))
  return(total)
  
}  


total_languages = c()

for(i in 1:nrow(org.data.DF))
{
  repos_url = paste("https://api.github.com/users/", org.data.DF[i,1], "/repos?per_page=100", sep = "")
  repo = GET(repos_url, gtoken)
  repo_cont = content(repo)
  repo.DF = jsonlite::fromJSON(jsonlite::toJSON(repo_cont))
  
  total_repos = repo.DF$name
  name = org.data.DF[i,1]
  total <- total_lang(name, total_repos)
  total_languages[length(total_languages)+1] = total
}

#Append them to the previous dataframe and update invalid location figures
# get public members

org.data.DF <- cbind(org.data.DF, total_languages, public_members)

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



#Visualise the Data:
plot(org.data.DF$Repos, org.data.DF$total_languages, main ="Number of Repos Vs Number of Languages", xlab="Repositories", ylab="Languages")


















# Stars and Forks of CERNS Repos

# Get Stars and Forks of Some Popular Github Repos

repos <- c("CSSEGISandData/COVID-19", "kubernetes/kubernetes", "Microsoft/vscode", "NixOS/nixpkgs", "Automattic/wp-calypso",
           "")

# Visualisation 1: Location of everyone who forked John Hopkin's Covid Data

