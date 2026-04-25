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
          title = tags$span(icon("database"), " Données utilisées & pipeline"),

          div(class = "data-schema",

            # ── Ligne 1 : Sources ─────────────────────────────────────────
            p(class = "schema-section-label", "SOURCES DE DONNÉES"),

            div(class = "schema-row",

              div(class = "source-card card-creg",
                div(class = "source-header",
                  span(class = "source-dot dot-creg"), "cRegMap"
                ),
                div(class = "source-files",
                  div(class = "file-entry",
                    span(class = "file-icon", "📄"),
                    div(
                      div(class = "file-name", "influence_uploadedData.csv"),
                      div(class = "file-dim", "70 lignées × 507 TF")
                    )
                  ),
                  div(class = "file-entry",
                    span(class = "file-icon", "📄"),
                    div(
                      div(class = "file-name", "classification_uploadedData.csv"),
                      div(class = "file-dim", "70 lignées → 7 sous-types")
                    )
                  )
                ),
                div(class = "source-badge", "Scores d'influence TF")
              ),

              div(class = "source-connector",
                div(class = "connector-line"),
                tags$span(class = "connector-arrow", HTML("&#8681;"))
              ),

              div(class = "source-card card-prism",
                div(class = "source-header",
                  span(class = "source-dot dot-prism"), "PRISM — Broad Institute"
                ),
                div(class = "source-files",
                  div(class = "file-entry",
                    span(class = "file-icon", "📄"),
                    div(
                      div(class = "file-name", "PRISM Repurposing Secondary.csv"),
                      div(class = "file-dim", "~727 lignées × 1 482 drogues")
                    )
                  ),
                  div(class = "file-entry",
                    span(class = "file-icon", "📄"),
                    div(
                      div(class = "file-name", "PRISM Viability Subsetted.csv"),
                      div(class = "file-dim", "Viabilité brute par dose")
                    )
                  )
                ),
                div(class = "source-badge", "AUC : aire sous courbe de viabilité")
              ),

              div(class = "source-connector",
                div(class = "connector-line"),
                tags$span(class = "connector-arrow", HTML("&#8681;"))
              ),

              div(class = "source-card card-depmap",
                div(class = "source-header",
                  span(class = "source-dot dot-depmap"), "DepMap — Broad Institute"
                ),
                div(class = "source-files",
                  div(class = "file-entry",
                    span(class = "file-icon", "📄"),
                    div(
                      div(class = "file-name", "Expression_Public_25Q3.csv"),
                      div(class = "file-dim", "~1 400 lignées × 19 000+ gènes")
                    )
                  )
                ),
                div(class = "source-badge", "RNA-seq normalisé (TPM log2)")
              )
            ),

            # ── Flèche ────────────────────────────────────────────────────
            div(class = "schema-arrow-down", HTML("&#8595;")),

            # ── Ligne 2 : Intersection ────────────────────────────────────
            p(class = "schema-section-label", "FILTRAGE & INTERSECTION"),

            div(class = "schema-row schema-row-center",
              div(class = "filter-box",
                div(class = "filter-row",
                  div(class = "filter-chip chip-teal", "70 lignées sein (cRegMap)"),
                  div(class = "filter-op", "∩"),
                  div(class = "filter-chip chip-blue", "PRISM (~727 lignées)"),
                  div(class = "filter-op", "="),
                  div(class = "filter-chip chip-orange", "30 lignées communes")
                ),
                div(class = "filter-note",
                  icon("circle-info"),
                  " PRISM couvre tous types de cancers — on ne garde que nos 70 lignées sein classifiées"
                )
              )
            ),

            # ── Flèche ────────────────────────────────────────────────────
            div(class = "schema-arrow-down", HTML("&#8595;")),

            # ── Ligne 3 : ML ──────────────────────────────────────────────
            p(class = "schema-section-label", "ENTRAÎNEMENT MACHINE LEARNING"),

            div(class = "schema-row schema-row-center",
              div(class = "ml-box",
                div(class = "ml-matrices",
                  div(class = "ml-matrix mat-x",
                    div(class = "ml-mat-title", "X — Features"),
                    div(class = "ml-mat-dim", "30 × 507"),
                    div(class = "ml-mat-desc", "Scores d'influence TF")
                  ),
                  div(class = "ml-plus", "+"),
                  div(class = "ml-matrix mat-y",
                    div(class = "ml-mat-title", "Y — Cible"),
                    div(class = "ml-mat-dim", "30 × N drogues"),
                    div(class = "ml-mat-desc", "AUC PRISM par drogue")
                  ),
                  div(class = "ml-arrow-right", HTML("&#8594;")),
                  div(class = "ml-model-box",
                    div(class = "ml-model-title", "Random Forest"),
                    div(class = "ml-model-sub", "1 modèle par drogue"),
                    div(class = "ml-model-sub", "Évaluation : LOO-CV (n = 30)")
                  )
                )
              )
            ),

            # ── Flèche ────────────────────────────────────────────────────
            div(class = "schema-arrow-down", HTML("&#8595;")),

            # ── Ligne 4 : Application ─────────────────────────────────────
            p(class = "schema-section-label", "APPLICATION"),

            div(class = "schema-row schema-row-center",
              div(class = "app-output-card",
                div(class = "app-out-icon", "🔬"),
                div(class = "app-out-label", "Prédiction ML"),
                div(class = "app-out-desc",
                  "Nouveau profil TF (507 scores)",
                  tags$br(),
                  HTML("&darr; Random Forest &darr;"),
                  tags$br(),
                  "Top médicaments prédits (AUC)"
                )
              ),
              div(class = "app-output-sep", "+"),
              div(class = "app-output-card",
                div(class = "app-out-icon", "🤖"),
                div(class = "app-out-label", "Hypothèses LLM"),
                div(class = "app-out-desc",
                  "Sous-type + top 5 drogues",
                  tags$br(),
                  HTML("&darr; Groq Llama 3.1 &darr;"),
                  tags$br(),
                  "Pistes biologiques mécanistiques"
                )
              )
            )

          ) # end data-schema
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
