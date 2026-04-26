# ── Helper function to compute responsive based on threshold ─────────────────
compute_responsive <- function(data, threshold) {
  data %>%
    mutate(responsive = AUC < threshold)
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

tab3_eda_ui <- function() {
  tabItem(tabName = "tab_eda",
          
          # ── AUC Threshold Slider ───────────────────────────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Paramètres d'analyse",
                sliderInput("auc_threshold", 
                            label = "Seuil de réponse (AUC) :",
                            min = 0.1, max = 0.7, value = 0.3, step = 0.01,
                            helpText("Une lignée est considérée 'répondante' si AUC < seuil")),
                div(class = "text-muted", 
                    "Plus le seuil est bas, plus la définition de 'réponse' est stricte.")
            )
          ),
          
          # ── 1. Distribution globale des AUC ─────────────────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Distribution globale des AUC",
                plotlyOutput("eda_hist_auc", height = "360px")
            )
          ),
          
          # ── Statistiques par cluster (sous l'histogramme) ────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Statistiques par cluster",
                DTOutput("eda_cluster_stats")
            )
          ),
          
          # ── 2. Boxplots AUC par cluster ──────────────────────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Distribution des AUC par cluster",
                plotlyOutput("eda_box_cluster", height = "420px")
            )
          ),
          
          # ── 3. Lignées & tests par cluster ───────────────────────────────────────
          fluidRow(
            box(width = 6, status = "success", solidHeader = TRUE,
                title = "Lignées cellulaires par cluster",
                plotlyOutput("eda_bar_celllines", height = "300px")
            ),
            box(width = 6, status = "success", solidHeader = TRUE,
                title = "Tests effectués par cluster",
                plotlyOutput("eda_bar_tests", height = "300px")
            )
          ),
          
          # ── 4. Top 10 médicaments — sélecteur de cluster ─────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Top 10 médicaments par cluster",
                
                fluidRow(
                  column(4,
                         selectInput("eda_cluster_select", label = "Choisir un cluster :",
                                     choices  = NULL,  # Will be set in server
                                     selected = NULL)
                  )
                ),
                
                fluidRow(
                  column(6,
                         h5("AUC moyenne ↓"),
                         plotlyOutput("eda_top10_mean", height = "400px")
                  ),
                  column(6,
                         h5(textOutput("prop_title")),  # Dynamic title with threshold
                         plotlyOutput("eda_top10_prop", height = "400px")
                  )
                )
            )
          ),
          
          # ── 5. Heatmap ───────────────────────────────────────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Heatmap AUC moyenne — Top médicaments × Clusters",
                plotlyOutput("eda_heatmap", height = "700px")
            )
          ),
          
          # ── 6. Tests statistiques ─────────────────────────────────────────────────
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Tests statistiques — Kruskal-Wallis & comparaisons par paires (BH)",
                verbatimTextOutput("eda_stat_tests")
            )
          )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

tab3_eda_server <- function(input, output, session) {
  
  # Reactive expression for threshold
  auc_threshold <- reactive({
    input$auc_threshold %||% DEFAULT_AUC_THRESHOLD
  })
  
  # Reactive: full data with responsive based on current threshold
  df_with_responsive <- reactive({
    req(df)
    compute_responsive(df, auc_threshold())
  })
  
  # Update cluster selector choices
  observe({
    updateSelectInput(session, "eda_cluster_select",
                      choices = levels(df$cluster),
                      selected = levels(df$cluster)[1])
  })
  
  # Reactive: cluster summary
  cluster_summary <- reactive({
    df_with_responsive() %>%
      group_by(cluster) %>%
      summarise(
        n_cell_lines = n_distinct(Cell.lines),
        n_tests      = n(),
        mean_AUC     = round(mean(AUC),   4),
        median_AUC   = round(median(AUC), 4),
        n_responsive = sum(responsive),
        pct_responsive = round(100 * n_responsive / n(), 1),
        .groups = "drop"
      )
  })
  
  # Reactive: top 10 by mean AUC
  top10_mean <- reactive({
    df_with_responsive() %>%
      group_by(cluster, drug_label) %>%
      summarise(mean_AUC = mean(AUC), sd_AUC = sd(AUC), n_cells = n(), .groups = "drop") %>%
      filter(n_cells >= MIN_CELLS) %>%
      group_by(cluster) %>%
      slice_min(mean_AUC, n = 10) %>%
      ungroup()
  })
  
  # Reactive: top 10 by proportion responsive
  prop_data <- reactive({
    df_with_responsive() %>%
      filter(!is.na(AUC)) %>%
      group_by(cluster, drug_label) %>%
      summarise(n_total = n(), n_resp = sum(responsive), prop = n_resp / n_total,
                .groups = "drop") %>%
      filter(n_total >= MIN_CELLS) %>%
      group_by(cluster) %>%
      slice_max(order_by = prop, n = 10, with_ties = FALSE) %>%
      arrange(cluster, prop)
  })
  
  # Dynamic title for proportion plot
  output$prop_title <- renderText({
    paste0("Proportion répondantes (AUC < ", round(auc_threshold(), 2), ") ↑")
  })
  
  # ── Histogramme AUC global ─────────────────────────────────────────────────
  output$eda_hist_auc <- renderPlotly({
    data <- df_with_responsive()
    auc_median <- median(data$AUC, na.rm = TRUE)
    threshold <- auc_threshold()
    
    plot_ly(x = ~data$AUC, type = "histogram", nbinsx = 80,
            marker = list(color = "#457B9D",
                          line  = list(color = "white", width = 0.5))) %>%
      layout(
        title  = "<b>Distribution globale des AUC</b>",
        xaxis  = list(title = "AUC", range = c(0, 1)),
        yaxis  = list(title = "Fréquence"),
        bargap = 0.05,
        shapes = list(
          list(type = "line", x0 = auc_median, x1 = auc_median,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "#2A9D8F", width = 2, dash = "dash")),
          list(type = "line", x0 = threshold, x1 = threshold,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "#E63946", width = 2, dash = "dot"))
        ),
        annotations = list(
          list(x = auc_median, y = 0.95, yref = "paper",
               text = paste("Médiane :", round(auc_median, 3)),
               showarrow = FALSE, xanchor = "left",
               font = list(color = "#2A9D8F", size = 13)),
          list(x = threshold, y = 0.90, yref = "paper",
               text = paste("Seuil :", round(threshold, 2)),
               showarrow = FALSE, xanchor = "left",
               font = list(color = "#E63946", size = 13))
        )
      )
  })
  
  # ── Table statistiques par cluster ────────────────────────────────────────
  output$eda_cluster_stats <- renderDT({
    cluster_summary() %>%
      rename(Cluster = cluster, "Nb lignées" = n_cell_lines,
             "Nb tests" = n_tests, "AUC moy." = mean_AUC, 
             "Médiane" = median_AUC, "Nb répondantes" = n_responsive,
             "% répondantes" = pct_responsive)
  }, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  
  # ── Boxplots AUC par cluster ───────────────────────────────────────────────
  output$eda_box_cluster <- renderPlotly({
    data <- df_with_responsive()
    colors_vec <- unname(CLUSTER_COLORS[levels(data$cluster)])
    plot_ly(data, x = ~cluster, y = ~AUC, color = ~cluster,
            colors = colors_vec, type = "box",
            boxpoints = "outliers", notched = TRUE,
            text = ~paste("Lignée :", Cell.lines, "<br>Drogue :", drug_label)) %>%
      layout(
        title = "<b>Distribution des AUC par cluster</b>",
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "AUC"),
        showlegend = TRUE
      )
  })
  
  # ── Barplots lignées & tests ───────────────────────────────────────────────
  output$eda_bar_celllines <- renderPlotly({
    summ <- cluster_summary()
    plot_ly(summ, x = ~cluster, y = ~n_cell_lines, type = "bar",
            marker = list(color = unname(CLUSTER_COLORS[summ$cluster]),
                          line  = list(color = "white", width = 1)),
            text = ~n_cell_lines, textposition = "outside") %>%
      layout(xaxis = list(title = "Cluster"),
             yaxis = list(title = "Nb lignées",
                          range = c(0, max(summ$n_cell_lines) * 1.2)),
             showlegend = FALSE)
  })
  
  output$eda_bar_tests <- renderPlotly({
    summ <- cluster_summary()
    plot_ly(summ, x = ~cluster, y = ~n_tests, type = "bar",
            marker = list(color = unname(CLUSTER_COLORS[summ$cluster]),
                          line  = list(color = "white", width = 1)),
            text = ~format(n_tests, big.mark = ","), textposition = "outside") %>%
      layout(xaxis = list(title = "Cluster"),
             yaxis = list(title = "Nb tests"),
             showlegend = FALSE)
  })
  
  # ── Top 10 : réactif au cluster sélectionné ───────────────────────────────
  
  output$eda_top10_mean <- renderPlotly({
    cl  <- input$eda_cluster_select
    req(cl)
    sub <- top10_mean() %>%
      filter(cluster == cl) %>%
      arrange(mean_AUC) %>%
      mutate(drug_label = factor(drug_label, levels = drug_label))
    
    n_cl <- cluster_summary() %>% filter(cluster == cl) %>% pull(n_cell_lines)
    threshold <- auc_threshold()
    
    plot_ly(sub, y = ~drug_label, x = ~mean_AUC,
            type = "bar", orientation = "h",
            marker = list(color = CLUSTER_COLORS[cl], opacity = 0.85,
                          line  = list(color = "white", width = 0.5)),
            hovertemplate = paste0("<b>%{y}</b><br>AUC moy = %{x:.3f}<br>",
                                   "n lignées = %{customdata}<extra></extra>"),
            customdata   = ~n_cells,
            text         = ~sprintf("%.3f (n=%d)", mean_AUC, n_cells),
            textposition = "outside") %>%
      layout(
        title  = sprintf("<b>%s</b> — %d lignée(s)", cl, n_cl),
        xaxis  = list(title = "AUC moyenne", range = c(0, 1.1)),
        yaxis  = list(title = "", autorange = "reversed"),
        shapes = list(list(type = "line",
                           x0 = threshold, x1 = threshold,
                           y0 = -0.5, y1 = nrow(sub) - 0.5,
                           line = list(color = "gray", dash = "dot", width = 1.5))),
        showlegend = FALSE)
  })
  
  output$eda_top10_prop <- renderPlotly({
    cl     <- input$eda_cluster_select
    req(cl)
    sub_df <- prop_data() %>%
      filter(cluster == cl) %>%
      arrange(prop) %>%
      mutate(drug_label = factor(drug_label, levels = drug_label))
    
    plot_ly(sub_df,
            x = ~prop * 100, y = ~drug_label,
            type = "bar", orientation = "h",
            marker = list(color = CLUSTER_COLORS[cl]),
            hovertemplate = paste0("<b>%{y}</b><br>%{x:.1f}% — ",
                                   sub_df$n_resp, "/", sub_df$n_total,
                                   "<extra></extra>")) %>%
      layout(xaxis = list(title = "% Répondantes", range = c(0, 105)),
             yaxis = list(title = ""),
             showlegend = FALSE)
  })
  
  # ── Heatmap ────────────────────────────────────────────────────────────────
  output$eda_heatmap <- renderPlotly({
    threshold <- auc_threshold()
    drugs_selected <- unique(top10_mean()$drug_label)
    mat_df <- df_with_responsive() %>%
      filter(drug_label %in% drugs_selected) %>%
      group_by(drug_label, cluster) %>%
      summarise(mean_AUC = mean(AUC), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = cluster, values_from = mean_AUC)
    mat_values           <- as.matrix(mat_df[, -1])
    rownames(mat_values) <- mat_df$drug_label
    annot <- matrix(
      ifelse(is.na(mat_values), "ND",
             ifelse(mat_values < threshold,
                    sprintf("%.2f★", mat_values),
                    sprintf("%.2f",  mat_values))),
      nrow = nrow(mat_values))
    plot_ly(z = mat_values, x = colnames(mat_values), y = rownames(mat_values),
            type = "heatmap",
            height = max(500, nrow(mat_values) * 22 + 150),
            colorscale = list(c(0, "green"), c(0.3, "yellow"), c(1, "red")),
            zmin = 0.3, zmax = 1.0,
            text = annot, texttemplate = "%{text}",
            hovertemplate = "<b>%{y}</b><br>%{x} — AUC moy = %{z:.3f}<extra></extra>",
            colorbar = list(title = "AUC moy")) %>%
      layout(
        title = list(text = sprintf("<b>Heatmap AUC × Clusters</b><br><sup>★ AUC < %.1f | ND = non testé</sup>",
                                    threshold), x = 0.5),
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "", autorange = "reversed"))
  })
  
  # ── Tests statistiques ────────────────────────────────────────────────────
  output$eda_stat_tests <- renderPrint({
    data <- df_with_responsive()
    cat("=== KRUSKAL-WALLIS (global) ===\n")
    kw <- kruskal.test(AUC ~ cluster, data = data)
    cat(sprintf("H = %.4f  |  df = %d  |  p-value = %.2e\n\n",
                kw$statistic, kw$parameter, kw$p.value))
    if (kw$p.value < 0.05) {
      cat("Comparaisons par paires — Wilcoxon, correction BH :\n\n")
      pw <- pairwise.wilcox.test(data$AUC, data$cluster, p.adjust.method = "BH")
      print(pw$p.value)
    } else {
      cat("Résultat non significatif (p >= 0.05).\n")
    }
  })
}