# Run from the planteam checkout root. The app binds only to this computer.
for (package in c("shiny", "jsonlite", "yaml")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install the app dependency: ", package, call. = FALSE)
}
if (!file.exists("app/app.R")) stop("Run from the planteam checkout root.", call. = FALSE)
shiny::runApp("app", host = "127.0.0.1", port = 3847, launch.browser = FALSE)
