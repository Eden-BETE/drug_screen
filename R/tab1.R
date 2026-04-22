# ══════════════════════════════════════════════════════════════════════════════
# tab1.R — Onglet Exploration & Présentation
# À compléter par : Théo (intro), Eden (données), Ania (PCA), Belkis (drogues)
# ══════════════════════════════════════════════════════════════════════════════

tab1_ui <- function() {
  tabItem(tabName = "tab_explore",

    # ── Section Théo : Introduction ──────────────────────────────────────────
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "🎗️ Cancer du sein — Introduction",
          div(class = "placeholder-section",
              icon("edit"), " Section à compléter par Théo",
              br(),
              "Présentation du cancer du sein, sous-types moléculaires,
               enjeux thérapeutiques, problématique du projet."
          )
      )
    ),

    # ── Section Eden : Données ───────────────────────────────────────────────
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = "📂 Présentation des données — Eden",
          div(class = "placeholder-section",
              icon("edit"), " Section à compléter par Eden",
              br(),
              "D'où viennent les données (DepMap, cRegMap, PRISM),
               structure des fichiers, nombre de lignées, nombre de drogues."
          )
      )
    ),

    # ── Section Ania : Visualisation des classes ─────────────────────────────
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "🔬 Visualisation des classes de lignées — Ania",
          div(class = "placeholder-section",
              icon("edit"), " Section à compléter par Ania",
              br(),
              "PCA sur les scores TF colorée par sous-type,
               heatmap d'expression ou d'influence par sous-type."
          )
      )
    ),

    # ── Section Belkis : Médicaments par sous-type ───────────────────────────
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = "💊 Médicaments efficaces par sous-type — Belkis",
          div(class = "placeholder-section",
              icon("edit"), " Section à compléter par Belkis",
              br(),
              "Distribution des AUC par sous-type, tests de Kruskal-Wallis,
               correction FDR, top drogues différentielles."
          )
      )
    )
  )
}

tab1_server <- function(input, output, session) {
  # Logique serveur à ajouter par chaque membre du groupe
}
