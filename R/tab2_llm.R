# ══════════════════════════════════════════════════════════════════════════════
# tab2_llm.R — Génération d'hypothèses biologiques via Groq (Llama 3)
# ══════════════════════════════════════════════════════════════════════════════

library(httr)
library(jsonlite)

# ── Appel API Groq ────────────────────────────────────────────────────────────
call_llm <- function(drug_table, subtype) {

  api_key <- Sys.getenv("GROQ_API_KEY")

  if (is.null(drug_table) || nrow(drug_table) == 0)
    return("Aucun résultat de prédiction à interpréter.")
  if (api_key == "")
    return("Clé API Groq manquante — vérifiez le fichier .Renviron.")

  drugs_str <- paste(drug_table$Médicament[1:min(5, nrow(drug_table))],
                     collapse = ", ")

  prompt <- paste0(
    "Tu es un expert en oncologie moléculaire spécialisé dans le cancer du sein. ",
    "Un modèle de machine learning entraîné sur des scores d'influence de facteurs ",
    "de transcription (TF) a prédit que les médicaments suivants sont les plus ",
    "efficaces pour une tumeur de sous-type '", subtype, "' : ", drugs_str, ". ",
    "En 5 phrases maximum, génère des hypothèses biologiques sur POURQUOI ces ",
    "médicaments pourraient être particulièrement efficaces pour ce sous-type. ",
    "Mentionne les voies de signalisation impliquées (ex. PI3K, HER2, ER, TNBC...) ",
    "et propose une hypothèse mécanistique concrète. Réponds en français."
  )

  response <- tryCatch({
    POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        model    = "llama-3.1-8b-instant",
        messages = list(list(role = "user", content = prompt)),
        max_tokens  = 500L,
        temperature = 0.7
      ), auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) NULL)

  if (is.null(response))
    return("Erreur réseau — vérifiez votre connexion.")
  if (status_code(response) != 200) {
    detail <- tryCatch({
      content(response, as = "parsed")$error$message
    }, error = function(e) "détail indisponible")
    return(paste0("Erreur API Groq (code ", status_code(response), ") : ", detail))
  }

  content(response, as = "parsed")$choices[[1]]$message$content
}

# ── UI ────────────────────────────────────────────────────────────────────────
tab2_llm_ui <- function() {
  tagList(
    fluidRow(
      box(width = 4, status = "success", solidHeader = TRUE,
          title = "Hypothèses biologiques — Groq",

          p(style = "color:#555; font-size:13px;",
            "Le modèle analyse les médicaments prédits et le sous-type détecté",
            "pour générer des hypothèses mécanistiques."),

          br(),

          actionButton("run_llm", "Générer les hypothèses",
                       class = "btn-success btn-lg",
                       style = "width: 100%;")
      ),

      box(width = 8, status = "primary", solidHeader = TRUE,
          title = "Hypothèses biologiques générées",
          uiOutput("llm_output")
      )
    ),

    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = "Comment interpréter les hypothèses",
          p("Le LLM ne remplace pas un expert — il génère des ",
            tags$b("pistes biologiques plausibles"), " à partir des résultats ML."),
          p("Ces hypothèses peuvent être utilisées pour :"),
          tags$ul(
            tags$li("Orienter des expériences de validation in vitro"),
            tags$li("Explorer des combinaisons thérapeutiques"),
            tags$li("Comprendre le lien entre profil TF et sensibilité aux drogues")
          )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
tab2_llm_server <- function(input, output, session,
                             pred_results, detected_subtype) {

  llm_text <- eventReactive(input$run_llm, {
    withProgress(message = "Interrogation de Groq...", value = 0.5, {
      call_llm(pred_results(), detected_subtype())
    })
  })

  output$llm_output <- renderUI({
    if (!isTruthy(input$run_llm) || input$run_llm == 0) {
      return(tags$p(
        "Lancez d'abord une prédiction ML, puis cliquez sur 'Générer les hypothèses'.",
        style = "color: #999; font-style: italic; padding: 10px;"
      ))
    }
    req(llm_text())
    html <- llm_text()
    html <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", html)
    html <- gsub("\\*(.+?)\\*",       "<em>\\1</em>",         html)
    html <- gsub("\n", "<br>", html)
    tags$div(class = "llm-box", HTML(html))
  })
}
