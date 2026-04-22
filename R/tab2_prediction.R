# ══════════════════════════════════════════════════════════════════════════════
# tab2_prediction.R — Prédiction ML des réponses aux médicaments
# ══════════════════════════════════════════════════════════════════════════════

# ── Fonction de prédiction ────────────────────────────────────────────────────
predict_best_drugs <- function(tf_scores_new, models, top_n = 10) {
  tf_df <- as.data.frame(t(tf_scores_new))

  preds <- sapply(models, function(res) {
    m          <- res$model
    tfs_needed <- rownames(m$importance)
    tfs_ok     <- intersect(tfs_needed, colnames(tf_df))
    if (length(tfs_ok) < 10) return(NA)
    predict(m, tf_df[, tfs_ok, drop = FALSE])
  })

  preds <- preds[!is.na(preds)]
  ranked <- sort(preds)[1:min(top_n, length(preds))]

  data.frame(
    Rang       = seq_along(ranked),
    Médicament = names(ranked),
    AUC_prédit = round(ranked, 4),
    Efficacité = ifelse(ranked < 0.5, "🟢 Élevée",
                 ifelse(ranked < 0.7, "🟡 Modérée", "🔴 Faible"))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
tab2_prediction_ui <- function(X_train, subtypes) {
  tagList(

    # Inputs
    fluidRow(
      box(width = 5, status = "primary", solidHeader = TRUE,
          title = "Profil TF du patient",

          radioButtons("input_mode", "Mode de saisie :",
            choices = list(
              "Lignée existante (démo)" = "demo",
              "Uploader un fichier TF"  = "upload"
            )
          ),

          conditionalPanel(
            condition = "input.input_mode == 'demo'",
            selectInput("cell_demo", "Choisir une lignée :",
                        choices  = rownames(X_train),
                        selected = rownames(X_train)[1])
          ),

          conditionalPanel(
            condition = "input.input_mode == 'upload'",
            fileInput("tf_file",
                      "Fichier CSV (colonne 1 = noms TF, colonne 2 = scores) :",
                      accept = ".csv"),
            helpText("Obtenu via cRegMap à partir d'un RNA-seq.")
          ),

          hr(),
          numericInput("top_n", "Nombre de médicaments à afficher :",
                       value = 10, min = 3, max = 30),
          actionButton("run_pred", "🔬 Lancer la prédiction",
                       class = "btn-primary btn-lg",
                       style = "width: 100%; margin-top: 10px;")
      ),

      box(width = 3, status = "warning", solidHeader = TRUE,
          title = "Sous-type détecté",
          br(),
          uiOutput("subtype_badge"),
          br(),
          helpText("Classification issue de cRegMap.")
      ),

      box(width = 4, status = "info", solidHeader = TRUE,
          title = "ℹ️ Comment lire les résultats",
          tags$ul(
            tags$li("L'", tags$b("AUC"), " mesure la viabilité cellulaire
                     sous traitement."),
            tags$li(tags$b("AUC faible (≈ 0)"), " → les cellules meurent
                     → médicament ", tags$b("très efficace"), "."),
            tags$li(tags$b("AUC proche de 1"), " → les cellules survivent
                     → médicament ", tags$b("peu efficace"), "."),
            tags$li("On classe du plus efficace au moins efficace.")
          )
      )
    ),

    # Résultats
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = "Top médicaments prédits",
          DTOutput("drug_table")
      )
    ),

    # Visualisations
    fluidRow(
      box(width = 6, status = "primary", solidHeader = TRUE,
          title = "AUC prédits par médicament",
          plotOutput("auc_plot", height = "320px")
      ),

      box(width = 6, status = "info", solidHeader = TRUE,
          title = "Top TF prédictifs pour ces médicaments",
          plotOutput("tf_importance_plot", height = "320px")
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
tab2_prediction_server <- function(input, output, session,
                                   X_train, subtypes, rf_models) {

  # Profil TF actif
  tf_profile <- reactive({
    req(input$run_pred)
    isolate({
      if (input$input_mode == "demo") {
        vec <- as.numeric(X_train[input$cell_demo, ])
        names(vec) <- colnames(X_train)
        vec
      } else {
        req(input$tf_file)
        df  <- read.csv(input$tf_file$datapath, row.names = 1, header = TRUE)
        vec <- df[, 1]
        names(vec) <- rownames(df)
        vec
      }
    })
  })

  # Sous-type
  detected_subtype <- reactive({
    req(input$run_pred)
    isolate({
      if (input$input_mode == "demo") as.character(subtypes[input$cell_demo])
      else "Inconnu"
    })
  })

  output$subtype_badge <- renderUI({
    req(detected_subtype())
    colors <- c(
      "MES"  = "#E74C3C", "PN"   = "#3498DB", "CL-B" = "#27AE60",
      "CL-C" = "#9B59B6", "CL-A" = "#E67E22", "NL"   = "#1ABC9C",
      "PN-L" = "#F39C12"
    )
    st  <- detected_subtype()
    col <- if (st %in% names(colors)) colors[st] else "#95A5A6"
    tags$div(
      tags$span(st, class = "badge-subtype",
                style = paste0("background-color:", col, ";"))
    )
  })

  # Résultats ML
  pred_results <- eventReactive(input$run_pred, {
    withProgress(message = "Prédiction en cours...", value = 0.6, {
      predict_best_drugs(tf_profile(), rf_models, top_n = input$top_n)
    })
  })

  # Table
  output$drug_table <- renderDT({
    req(pred_results())
    datatable(pred_results(), rownames = FALSE,
              options = list(pageLength = 10, dom = "tip")) %>%
      formatStyle("AUC_prédit",
                  background = styleColorBar(c(0, 1), "#AED6F1"),
                  backgroundSize    = "100% 80%",
                  backgroundRepeat  = "no-repeat",
                  backgroundPosition = "center")
  })

  # Barplot AUC
  output$auc_plot <- renderPlot({
    req(pred_results())
    ggplot(pred_results(),
           aes(x = reorder(Médicament, -AUC_prédit),
               y = AUC_prédit, fill = AUC_prédit)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_gradient(low = "#27AE60", high = "#E74C3C", limits = c(0, 1)) +
      geom_hline(yintercept = 0.7, linetype = "dashed", color = "grey50") +
      labs(x = NULL, y = "AUC prédit",
           caption = "Vert = efficace  |  Rouge = peu efficace") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })

  # TF importance
  output$tf_importance_plot <- renderPlot({
    req(pred_results())
    top5 <- pred_results()$Médicament[1:min(5, nrow(pred_results()))]
    mods  <- rf_models[intersect(top5, names(rf_models))]
    if (length(mods) == 0) return(NULL)

    imp_df <- do.call(rbind, lapply(names(mods), function(d) {
      imp <- mods[[d]]$importance
      data.frame(TF = rownames(imp), Imp = imp[, "%IncMSE"], Drug = d)
    }))

    imp_df %>%
      group_by(TF) %>%
      summarise(Importance = mean(Imp, na.rm = TRUE)) %>%
      arrange(desc(Importance)) %>%
      head(15) %>%
      ggplot(aes(x = reorder(TF, Importance), y = Importance)) +
      geom_col(fill = "#2C3E50") +
      coord_flip() +
      labs(x = NULL, y = "Importance (%IncMSE)",
           subtitle = "Moyennée sur le top 5 médicaments prédits") +
      theme_minimal(base_size = 12)
  })

  # Exposer le sous-type pour tab2_llm.R
  return(list(
    pred_results     = pred_results,
    detected_subtype = detected_subtype
  ))
}
