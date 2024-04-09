library(shiny)
library(shinylive)
library(httpuv)


shinylive::export(
  appdir = "apps/hello_world/",
  destdir = "docs/hello_world/"
)


httpuv::runStaticServer(dir = "docs/hello_world/", port = 8888)

