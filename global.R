# ══════════════════════════════════════════════════════════════════════
# Təmir Tikinti İdarəsi — Paket yükləmə
# Shinyapps.io / Binder bu fayldan asılılıqları aşkarlayır
# ══════════════════════════════════════════════════════════════════════

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(pool)
library(DT)
library(dplyr)
library(plotly)
library(httr2)
library(jsonlite)
library(shinyjs)
library(shinycssloaders)
library(openxlsx)

# PostgreSQL varsa yüklə (Binder-da yoxdur)
tryCatch(library(RPostgres), error = function(e) message("RPostgres yoxdur — SQLite rejimi"))
