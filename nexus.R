## Default repo
local({r <- getOption("repos")
r["Nexus"] <- "https://nexus-nodejs/repository/RProxy"
options(repos=r)
})
