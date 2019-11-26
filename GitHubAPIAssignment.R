#-------------------------------------------------------
#James Nangle - 17338145 - Software Engineering Project#
#-------------------------------------------------------

#-------------------------------------------------------
# Interfacing With the Github API
#-------------------------------------------------------

1

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 

# Code sourced from https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08 tutorial.

userData = fromJSON("https://api.github.com/users/nangleja")

#-------------------------------------------------------
# Interrogating the results
#-------------------------------------------------------

userData$followers

myFollowers = fromJSON("https://api.github.com/users/nangleja/followers")

myFollowers$login