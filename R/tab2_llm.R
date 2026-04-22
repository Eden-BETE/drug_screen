# ══════════════════════════════════════════════════════════════════════════════
# tab2_llm.R — Génération d'hypothèses biologiques via LLM (OpenAI)
# ══════════════════════════════════════════════════════════════════════════════

library(httr)
library(jsonlite)

# ── Appel API OpenAI ──────────────────────────────────────────────────────────
call_llm <- function(drug_table, subtype, api_key) {

  if (is.null(drug_table) || nrow(drug_table) == 0)
    return("⚠️ Aucun résultat de prédiction à interpréter.")
  if (api_key == "" || is.null(api_key))
    return("⚠️ Clé API OpenAI manquante.")

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
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        model      = "gpt-4o-mini",
        messages   = list(list(role = "user", content = prompt)),
        max_tokens = 500,
        temperature = 0.7
      ), auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(response))
    return("⚠️ Erreur réseau — vérifiez votre connexion.")
  if (status_code(response) != 200)
    return(paste("⚠️ Erreur API (code", status_code(response),
                 ") — vérifiez votre clé OpenAI."))

  content(response, as = "parsed")$choices[[1]]$message$content
}

# ── UI ────────────────────────────────────────────────────────────────────────
tab2_llm_ui <- function() {
  tagList(
    fluidRow(
      box(width = 4, status = "success", solidHeader = TRUE,
          title = "🤖 Paramètres LLM",

          passwordInput("api_key",
                        "Clé API OpenAI :",
                        placeholder = "sk-..."),
          helpText("Votre clé reste locale et n'est jamais sauvegardée."),

          hr(),

          selectInput("llm_model", "Modèle :",
                      choices  = list("GPT-4o mini (rapide)" = "gpt-4o-mini",
                                      "GPT-4o (puissant)"    = "gpt-4o"),
                      selected = "gpt-4o-mini"),

          br(),
          actionButton("run_llm", "✨ Générer les hypothèses",
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
          title = "ℹ️ Comment interpréter les hypothèses",
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
    req(input$api_key)
    withProgress(message = "Interrogation du LLM...", value = 0.5, {
      call_llm(pred_results(), detected_subtype(), input$api_key)
    })
  })

  output$llm_output <- renderUI({
    if (!isTruthy(input$run_llm) || input$run_llm == 0) {
      return(tags$p(
        "👆 Lancez d'abord une prédiction, entrez votre clé API,
         puis cliquez sur 'Générer les hypothèses'.",
        style = "color: #999; font-style: italic; padding: 10px;"
      ))
    }
    req(llm_text())
    tags$div(class = "llm-box", llm_text())
  })
}
