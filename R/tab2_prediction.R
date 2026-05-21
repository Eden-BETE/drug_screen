# ══════════════════════════════════════════════════════════════════════════════
# tab2_prediction.R — Prédiction ML des réponses aux médicaments
# ══════════════════════════════════════════════════════════════════════════════
library(stringr)

# ── Fonction de prédiction ────────────────────────────────────────────────────
predict_best_drugs <- function(tf_scores_new, models, top_n = 10) {
  tf_df <- as.data.frame(t(tf_scores_new))

  preds <- sapply(names(models), function(d) {
    res        <- models[[d]]
    m          <- res$model
    tfs_needed <- rownames(m$importance)
    tfs_ok     <- intersect(tfs_needed, colnames(tf_df))
    if (length(tfs_ok) < 10) return(NA)
    as.numeric(predict(m, tf_df[, tfs_ok, drop = FALSE]))
  })

  preds  <- preds[!is.na(preds)]
  ranked <- sort(preds)[1:min(top_n, length(preds))]

  data.frame(
    Rang       = seq_along(ranked),
    Médicament = names(ranked),
    AUC_prédit = round(ranked, 4),
    Efficacité = ifelse(ranked < 0.3, "🟢 Élevée",
                 ifelse(ranked < 0.5, "🟡 Modérée", "🔴 Faible")),
    stringsAsFactors = FALSE
  )
}

# ── Helper : graphe vide stylé ────────────────────────────────────────────────
empty_plot <- function(msg = "Lancez une prédiction pour afficher ce graphe") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg,
             color = "#c5cdd9", size = 4.5, hjust = 0.5, vjust = 0.5,
             fontface = "italic") +
    theme_void() +
    theme(plot.background = element_rect(fill = "#fafbfc", color = NA))
}

# ── UI ────────────────────────────────────────────────────────────────────────
tab2_prediction_ui <- function(X_train, subtypes) {
  tagList(

    # ── Ligne 1 : Inputs + Sous-type + Légende ────────────────────────────
    fluidRow(

      box(width = 5, status = "primary", solidHeader = TRUE,
          title = tags$span(icon("dna"), " Profil TF du patient"),

          radioButtons("input_mode", NULL,
            choices = list(
              "Lignée existante (démo)" = "demo",
              "Uploader un fichier TF"  = "upload"
            ),
            inline = TRUE
          ),

          conditionalPanel(
            condition = "input.input_mode == 'demo'",
            selectInput("cell_demo", "Lignée cellulaire :",
                        choices  = rownames(X_train),
                        selected = rownames(X_train)[1])
          ),

          conditionalPanel(
            condition = "input.input_mode == 'upload'",
            fileInput("tf_file",
                      "Fichier CSV (col. 1 = TF, col. 2 = scores) :",
                      accept = ".csv"),
            helpText(icon("info-circle"),
                     " Scores d'influence calculés via cRegMap à partir d'un RNA-seq.")
          ),

          hr(style = "margin: 10px 0;"),

          fluidRow(
            column(6,
              numericInput("top_n", "Nb de médicaments :",
                           value = 10, min = 3, max = 30)
            ),
            column(6, br(),
              actionButton("run_pred", "Lancer",
                           icon  = icon("flask"),
                           class = "btn-primary btn-lg",
                           style = "width:100%; margin-top:4px;")
            )
          )
      ),

      box(width = 3, status = "warning", solidHeader = TRUE,
          title = tags$span(icon("tag"), " Sous-type détecté"),
          div(style = "text-align:center; min-height:100px; padding:8px 0;",
              uiOutput("subtype_badge"))
      ),

      box(width = 4, status = "info", solidHeader = TRUE,
          title = tags$span(icon("circle-info"), " Lecture des résultats"),
          div(class = "info-panel",
            div(class = "info-row",
              div(class = "auc-chip", style = "background:#27AE60;", "AUC < 0.3"),
              span("Sensibilité élevée (forte mort cellulaire)")
            ),
            div(class = "info-row",
              div(class = "auc-chip", style = "background:#E67E22;", "0.3 – 0.5"),
              span("Sensibilité modérée")
            ),
            div(class = "info-row",
              div(class = "auc-chip", style = "background:#E74C3C;", "AUC > 0.5"),
              span("Faible sensibilité")
            )
          )
      )
    ),

    # ── Résumé post-prédiction ─────────────────────────────────────────────
    uiOutput("pred_summary_row"),

    # ── Ligne 2 : Table ───────────────────────────────────────────────────
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = tags$span(icon("table"), " Top médicaments prédits"),
          DTOutput("drug_table")
      )
    ),

    # ── Ligne 3 : Graphes ─────────────────────────────────────────────────
    fluidRow(

      box(width = 12, status = "info", solidHeader = TRUE,
          title = tags$span(icon("brain"), " Top facteurs de transcription prédictifs"),
          plotOutput("tf_importance_plot", height = "360px")
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
        df  <- read.csv(input$tf_file$datapath, header = FALSE, sep = ";")
        vec <- as.numeric(df[, 2])
        names(vec) <- df[, 1]
        vec
      }
    })
  })

  # Sous-type détecté
  detected_subtype <- reactive({
    req(input$run_pred)
    isolate({
      if (input$input_mode == "demo") as.character(subtypes[input$cell_demo])
      else "Inconnu"
    })
  })

  # Badge sous-type
  output$subtype_badge <- renderUI({
    if (!isTruthy(input$run_pred) || input$run_pred == 0) {
      return(tags$p("Lancez une prédiction pour détecter le sous-type.",
                    style = "color:#ccc; font-style:italic; font-size:13px; padding-top:14px;"))
    }
    req(detected_subtype())
    colors <- c(
      "MES"  = "#E74C3C", "PN"   = "#3498DB", "CL-B" = "#27AE60",
      "CL-C" = "#9B59B6", "CL-A" = "#E67E22", "NL"   = "#1ABC9C",
      "PN-L" = "#F39C12"
    )
    st  <- detected_subtype()
    col <- if (st %in% names(colors)) colors[st] else "#95A5A6"
    tagList(
      tags$span(st, class = "badge-subtype",
                style = paste0("background-color:", col, ";")),
      br(), br(),
      tags$small(style = "color:#aaa; font-size:11px;", "Classification via cRegMap")
    )
  })

  # Résultats ML
  pred_results <- eventReactive(input$run_pred, {
    withProgress(message = "Prédiction en cours...", value = 0.6, {
      predict_best_drugs(tf_profile(), rf_models, top_n = input$top_n)
    })
  })

  # Cartes résumé
  output$pred_summary_row <- renderUI({
    if (!isTruthy(input$run_pred) || input$run_pred == 0) return(NULL)
    req(pred_results())
    df    <- pred_results()
    top1  <- str_trunc(df$Médicament[1], 28)
    auc1  <- df$AUC_prédit[1]
    n_eff <- sum(df$AUC_prédit < 0.5)

    fluidRow(
      column(4, div(class = "summary-card",
        div(class = "summary-icon", "💊"),
        div(class = "summary-label", "Meilleur médicament prédit"),
        div(class = "summary-value small-val", top1)
      )),
      column(4, div(class = "summary-card",
        div(class = "summary-icon", "📊"),
        div(class = "summary-label", "Meilleure AUC prédite"),
        div(class = "summary-value", round(auc1, 3))
      )),
      column(4, div(class = "summary-card",
        div(class = "summary-icon", "🟢"),
        div(class = "summary-label", "Médicaments potentiellement efficaces (AUC < 0.5)"),
        div(class = "summary-value", paste0(n_eff, " / ", nrow(df)))
      ))
    )
  })

  # Table
  output$drug_table <- renderDT({
    req(pred_results())
    datatable(
      pred_results(), rownames = FALSE,
      options = list(
        pageLength = 10, dom = "tip",
        columnDefs = list(list(className = "dt-center", targets = c(0, 2, 3)))
      )
    ) %>%
      formatStyle("AUC_prédit",
                  background         = styleColorBar(c(0, 1), "#AED6F1"),
                  backgroundSize     = "100% 70%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Efficacité", fontWeight = "bold") %>%
      formatStyle("Rang",
                  color      = "#2C3E50",
                  fontWeight = "600")
  })

  # Barplot AUC
  output$auc_plot <- renderPlot({
    if (!isTruthy(input$run_pred) || input$run_pred == 0)
      return(empty_plot())
    req(pred_results())
    df <- pred_results()
    df$Drug_label <- str_trunc(df$Médicament, 30)

    ggplot(df, aes(x = reorder(Drug_label, -AUC_prédit),
                   y = AUC_prédit, fill = AUC_prédit)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      # geom_hline (axe y continu) : après coord_flip, ces lignes deviennent verticales
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "#27AE60", alpha = 0.7) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "#E74C3C", alpha = 0.6) +
      geom_text(aes(label = sprintf("%.3f", AUC_prédit)),
                hjust = -0.15, size = 3.2, color = "#555", fontface = "bold") +
      coord_flip(clip = "off") +
      scale_fill_gradient2(
        low = "#27AE60", mid = "#F39C12", high = "#E74C3C",
        midpoint = 0.5, limits = c(0, 1)
      ) +
      scale_y_continuous(limits = c(0, 1.15),
                         breaks = c(0, 0.3, 0.5, 0.7, 1.0)) +
      labs(x = NULL, y = "AUC prédit",
           caption = "Seuil vert = 0.3  |  Seuil rouge = 0.5  (Corsello et al., 2020)") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_text(size = 10, color = "#2C3E50"),
        plot.caption       = element_text(color = "#aaa", size = 9, hjust = 0),
        plot.margin        = ggplot2::margin(5, 40, 10, 5)
      )
  })

  # TF importance
  output$tf_importance_plot <- renderPlot({
    if (!isTruthy(input$run_pred) || input$run_pred == 0)
      return(empty_plot())
    req(pred_results())

    top5  <- pred_results()$Médicament[1:min(5, nrow(pred_results()))]
    mods  <- rf_models[intersect(top5, names(rf_models))]

    # Fallback : si les noms ne correspondent pas, utiliser tous les modèles
    subtitle_txt <- if (length(mods) > 0) {
      "Moyennée sur le top 5 médicaments prédits"
    } else {
      mods <- rf_models
      "Moyennée sur l'ensemble des modèles entraînés"
    }

    # Extraire l'importance : essayer $model$importance (matrice) puis $importance (vecteur)
    get_imp <- function(m) {
      if (!is.null(m$importance) && length(m$importance) > 0) {
        imp <- m$importance
        data.frame(TF = names(imp), Imp = as.numeric(imp),
                   stringsAsFactors = FALSE)
      } else if (!is.null(m$model) && !is.null(m$model$importance)) {
        mat <- importance(m$model)
        data.frame(TF = rownames(mat), Imp = mat[, "%IncMSE"],
                   stringsAsFactors = FALSE)
      } else NULL
    }

    imp_list <- lapply(names(mods), function(d) {
      df_i <- get_imp(mods[[d]])
      if (!is.null(df_i)) { df_i$Drug <- d; df_i } else NULL
    })
    imp_df <- do.call(rbind, Filter(Negate(is.null), imp_list))

    if (is.null(imp_df) || nrow(imp_df) == 0)
      return(empty_plot("Importance TF non disponible"))

    imp_summary <- imp_df %>%
      group_by(TF) %>%
      summarise(Importance = mean(Imp, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Importance)) %>%
      head(15)

    if (nrow(imp_summary) == 0)
      return(empty_plot("Importance TF non disponible"))

    ggplot(imp_summary, aes(x = reorder(TF, Importance), y = Importance)) +
      geom_col(fill = "#2C3E50", width = 0.65) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Importance (%IncMSE)", subtitle = subtitle_txt) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.subtitle      = element_text(color = "#999", size = 9),
        axis.text.y        = element_text(size = 10, color = "#2C3E50"),
        plot.margin        = ggplot2::margin(5, 40, 10, 5)
      )
  })

  return(list(
    pred_results     = pred_results,
    detected_subtype = detected_subtype
  ))
}
