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


org.data.DF
  
#TODO: Create separate function to get total public members and total languages
#Append them to the previous dataframe and then also remove null/incorrect values for location
# get public members

org3 = GET("https://api.github.com/organizations/", org_user_list[i], sep = "", gtoken)
org_cont2 = content(org3)
org.DF2 <- jsonlite::fromJSON(jsonlite::toJSON(org_cont2))
public_members <- org.DF2$public_members



# Stars and Forks of CERNS Repos

# Get Stars and Forks of Some Popular Github Repos

repos <- c("CSSEGISandData/COVID-19", "kubernetes/kubernetes", "Microsoft/vscode", "NixOS/nixpkgs", "Automattic/wp-calypso",
           "")

# Visualisation 1: Location of everyone who forked John Hopkin's Covid Data





