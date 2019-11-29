#-------------------------------------------------------
#James Nangle - 17338145 - Software Engineering Project#
#-------------------------------------------------------

#-------------------------------------------------------
# Interfacing With the Github API
#-------------------------------------------------------

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)
#install.packages("devtools")
require(devtools)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")
# Change based on what your application is

# Change based on what you 
myapp <- oauth_app(appname = "NangleJaSoftEng2",
                   key = "e2572a78eb60670c4809",
                   secret = "f01833e329798fdde851b01ba394883e2f1b0669")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)


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


#-------------------------------------------------------
# Interrogating me GitHub
#-------------------------------------------------------

userData = fromJSON("https://api.github.com/users/nangleja")

userData$followers

myFollowers = fromJSON("https://api.github.com/users/nangleja/followers")

myFollowers$login

following = fromJSON("https://api.github.com/users/nangleja/following")
following$login #Lists the logins of people I'm following

userData$public_repos #Displays the number of repositories I have

repos = fromJSON("https://api.github.com/users/nangleja/repos")
repos$name #Details and print out the names of my public repositories
repos$created_at #Gives details of the date the repositories were created 
repos$full_name #gives names of repositories

#-----------------------------------------------------------------------
# Interrogating Johno (One of the trending github users this month)
#-----------------------------------------------------------------------

johnData = GET("https://api.github.com/users/johno/followers?per_page=100;", gtoken)
stop_for_status(johnData)
extract = content(johnData)

githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login

# List of usernames
id = githubDB$login
user_ids = c(id)
user_ids

# Creates dataframe of users
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

#loops through users and adds to list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Does not add users if they have no followers
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loop through 'following' users
  for (j in 1:length(followingLogin))
  {
    #Check for duplicate users
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Adds user to the current list
      users[length(users) + 1] = followingLogin[j]
      
      #Obtain information from each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieves who user is following
      followingNumber = followingDF2$following
      
      #Retrieves users followers
      followersNumber = followingDF2$followers
      
      #Retrieves how many repository the user has 
      reposNumber = followingDF2$public_repos
      
      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #Stop when there are more than n users
  if(length(users) > 100)
  {
    break
  }
  next
}

Sys.setenv("plotly_username"="nangleja")
Sys.setenv("plotly_api_key"="TWtEdCGKdbGZmBBTlG3d")

#plot one graphs repositories vs followers coloured by year
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated, colors = "Set3")
plot1


#plot two graphs following vs followers again coloured by year
plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated, colors = "Set3")
plot2

api_create(plot1, filename = "GitHub API - Repositories vs Followers for 100 users")
api_create(plot2, filename = "GitHub API - Following vs Followers for 100 users")

#-----------------------------------------------------------------------
# Top languages research
#-----------------------------------------------------------------------

languages = c()
#Scraping the followers of Johno and researching the most common languages used.

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through all the repositories of an individual user
  if(length(RepositoriesNames) < 10){
    reps = length(RepositoriesNames)
  }
  else{
    reps = 10
  }
  
  for (j in 1: reps)
  {
    
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
    #Removes repositories containing no specific languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#Puts the most popular languages in table: 
allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-19):length(allLanguages)]

#converts to dataframe
languageDF = as.data.frame(top10Languages)

#Plot the data frame of languages
plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3


api_create(plot3, filename = "20 Most Popular Languages")

