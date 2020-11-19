install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)


oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "REST_API_Visualisation",
                   key = "7471d2a0322c6e108c48",
                   secret = "f648b73c315cd4f65ec16f025effd568d9de45c0")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jina101/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jina101/datasharing"] 

