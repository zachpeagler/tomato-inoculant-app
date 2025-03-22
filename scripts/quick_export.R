#quick and dirty deployment script
shinylive::export("C:/Github/tomato-inoculant-app/app", "C:/Github/tomato-inoculant-app/site")

# profiling
profvis::profvis({
  shiny::runApp("C:/Github/tomato-inoculant-app/app")
})
