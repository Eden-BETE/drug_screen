library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(ggplot2)
library(DT)

# Charger les variables d'environnement (GROQ_API_KEY, etc.)
if (file.exists(".Renviron")) readRenviron(".Renviron")

# ── Sources modulaires ────────────────────────────────────────────────────────
source("R/tab1.R")
source("R/tab2_prediction.R")
source("R/tab2_llm.R")

# ── Chargement des modèles ────────────────────────────────────────────────────
# Générer ces fichiers en knittant ML_BCA_prediction.Rmd d'abord
rf_models <- readRDS("models/rf_models.rds")
X_train   <- readRDS("models/X_train.rds")
subtypes  <- readRDS("models/subtypes.rds")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span("🎗️ BCA Drug Predictor")
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploration & Données", tabName = "tab_explore",
               icon = icon("chart-bar")),
      menuItem("Prédiction ML + LLM",  tabName = "tab_pred",
               icon = icon("brain"))
    )
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "styles.css")),

    tabItems(
      # Tab 1 — Exploration
      tab1_ui(),

      # Tab 2 — Prédiction ML + LLM dans le même onglet
      tabItem(tabName = "tab_pred",
        tab2_prediction_ui(X_train, subtypes),  # R/tab2_prediction.R
        hr(),
        tab2_llm_ui()                           # R/tab2_llm.R
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # Tab 1
  tab1_server(input, output, session)

  # Prédiction ML — retourne pred_results + detected_subtype pour le LLM
  pred_out <- tab2_prediction_server(input, output, session,
                                     X_train, subtypes, rf_models)

  # LLM — dans le même onglet, reçoit les résultats de prédiction
  tab2_llm_server(input, output, session,
                  pred_results     = pred_out$pred_results,
                  detected_subtype = pred_out$detected_subtype)
}

shinyApp(ui, server)
