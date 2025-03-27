# set wd
setwd("C:/Github/tomato-inoculant-app/app")

#quick and dirty deployment script
setwd("C:/Github/tomato-inoculant-app")
shinylive::export("app", "site")

# profiling
profvis::profvis({
  shiny::runApp("C:/Github/tomato-inoculant-app/app")
})
