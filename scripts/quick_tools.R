#quick and dirty deployment script
shinylive::export("C:/Github/tomato-inoculant-app/app", "C:/Github/tomato-inoculant-app/site")

# set wd
setwd("C:/Github/tomato-inoculant-app/app")

# profiling
profvis::profvis({
  shiny::runApp("C:/Github/tomato-inoculant-app/app")
})
