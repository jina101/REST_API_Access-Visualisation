install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)

usethis::edit_r_environ()


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