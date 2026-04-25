# ══════════════════════════════════════════════════════════════════════════════
# tab1.R — Onglet Exploration & Présentation
# À compléter par : Théo (intro), Eden (données), Ania (PCA), Belkis (drogues)
# ══════════════════════════════════════════════════════════════════════════════

tab1_ui <- function() {
  tabItem(tabName = "tab_explore",

    # ── Section Théo : Introduction ──────────────────────────────────────────
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "🎗️ Cancer du sein — Introduction & Problématique",

          # Accroche
          div(class = "intro-accroche",
            tags$strong("~3 millions de nouveaux cas par an"),
            " — le cancer du sein est le cancer féminin le plus prévalent. Sa particularité :
            ce n'est pas une seule maladie, mais une",
            tags$strong("mosaïque de sous-types moléculaires"),
            "aux profils d'expression génique et aux réponses thérapeutiques très différentes."
          ),

          # Badges sous-types
          div(class = "intro-subtypes",
            p(style = "color:#777; font-size:13px; margin-bottom:10px;",
              "7 sous-types identifiés à partir de 70 lignées cellulaires via",
              tags$strong("cRegMap"), "(scores d'influence des facteurs de transcription) :"),
            tags$span(class = "subtype-label", style = "background:#E74C3C;", "MES"),
            tags$span(class = "subtype-label", style = "background:#3498DB;", "PN"),
            tags$span(class = "subtype-label", style = "background:#E67E22;", "CL-A"),
            tags$span(class = "subtype-label", style = "background:#27AE60;", "CL-B"),
            tags$span(class = "subtype-label", style = "background:#9B59B6;", "CL-C"),
            tags$span(class = "subtype-label", style = "background:#1ABC9C;", "NL"),
            tags$span(class = "subtype-label", style = "background:#F39C12;", "PN-L")
          ),

          hr(style = "margin: 16px 0;"),

          # Problématique en 2 cartes
          fluidRow(
            column(6,
              div(class = "intro-card",
                div(class = "intro-card-icon", "🤖"),
                tags$h4("Prédire"),
                p("Étant donné le profil d'activité des facteurs de transcription d'une tumeur,
                  quels médicaments seront les plus efficaces ?")
              )
            ),
            column(6,
              div(class = "intro-card",
                div(class = "intro-card-icon", "💬"),
                tags$h4("Expliquer"),
                p("Générer automatiquement des hypothèses biologiques qui justifient
                  ces prédictions pour le sous-type détecté.")
              )
            )
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
