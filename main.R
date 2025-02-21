library(shiny)
library(shinylive)
library(httpuv)


# 0. 헬로월드 -----------------------------------------------------------------

shinylive::export(
  appdir = "apps/hello_world/",
  destdir = "docs/hello_world/"
)

httpuv::runStaticServer(dir = "docs/hello_world/", port = 8888)

# 1. 모집단 - 표본추출 -----------------------------------------------------------

shinylive::export(
  appdir = "apps/sampling/",
  destdir = "docs/sampling/"
)

httpuv::runStaticServer(dir = "docs/sampling/", port = 8881)

## 1.1. 모집단 - 표본추출 (영어) -----------------------------------------------------------

shinylive::export(
  appdir = "apps/sampling_en/",
  destdir = "docs/sampling_en/"
)

httpuv::runStaticServer(dir = "docs/sampling_en/", port = 8881)

# 2. 기술통계 -----------------------------------------------------------

shinylive::export(
  appdir = "apps/descriptive",
  destdir = "docs/descriptive"
)

httpuv::runStaticServer(dir = "docs/descriptive", port = 8881)

# 2.1. 기술통계 (영어) --------------------------------------------------

shinylive::export(
  appdir = "apps/descriptive_en",
  destdir = "docs/descriptive_en"
)

httpuv::runStaticServer(dir = "docs/descriptive_en", port = 8881)
