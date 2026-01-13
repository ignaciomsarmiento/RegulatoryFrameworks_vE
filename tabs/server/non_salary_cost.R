labor_server <- function(input, output, session) {
  
  # COLORS
  
  # Total

  ns <- session$ns
  tabla <- readRDS("data/non_salary/bonuses_and_benefits_component.rds")
  
  # ---- Non Salary Tables and adding tenure  ----
  df_non_salary <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
  wage_levels <- c("1sm", "2sm", "5sm", "10sm", "15sm")
  wage_labels <- paste0(sub("sm", "", wage_levels), " MW")
  wage_choices <- setNames(wage_levels, wage_labels)
  
  
  # non_salary variables
  ns_variables<-reactiveValues(
    order_country=NULL,
    country_sel="All",
    countries=c("All",unique(df_non_salary$country)),
    df_final=NULL,
    df_final_tabla=NULL
  )
  
  
  df_non_salary_payer <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
  
  df_non_salary_component <- readRDS("data/non_salary/total_ns_costs_by_component.rds")
  
  # ---- Selection Groups: Button results ----
  selected_group0 <- reactiveVal("all") # First Filter
  selected_groupA <- reactiveVal("total") # Total, by Payer, By Component   
  selected_groupB <- reactiveVal("1sm") # 1 MW / 2 MW / 5 MW / 10 MW / 15 MW
  selected_groupC <- reactiveVal("all_component")
  selected_groupD <- reactiveVal("all_bonuses")
  selected_groupE <- reactiveVal("pensions")
  option1_selected <- reactiveVal(FALSE)
  table_visible <- reactiveVal(FALSE)
  last_country_selection <- reactiveVal("All")
  last_wage_selection <- reactiveVal("1sm")
  last_single_country <- reactiveVal(NULL)
  last_compare_mode <- reactiveVal("country")
  option1_selected(TRUE)

  reset_across_defaults <- function() {
    ns_variables$country_sel <- "All"
    ns_variables$order_country <- NULL
    last_country_selection("All")
    last_wage_selection("1sm")
    last_single_country(NULL)
    selected_group0("all")
    selected_groupA("total")
    selected_groupB("1sm")
    selected_groupC("all_component")
    selected_groupD("all_bonuses")
    selected_groupE("pensions")
    option1_selected(TRUE)
    updateSelectizeInput(session, ns("mw_selection"), selected = "1sm")
    updateSelectizeInput(session, ns("country_selection_user"), selected = "All")
    shinyjs::runjs(sprintf("$('#%s').click();", ns("all")))
  }
  plotly_font_family <- "National Park, 'Source Sans Pro', -apple-system, BlinkMacSystemFont, sans-serif"
  component_palette <- c(
    "Pension" = "#00C1FF",
    "Health" = "#002244",
    "Occupational Risk" = "#B9BAB5",
    "Bonuses and Benefits" = "#335B8E",
    "Payroll Taxes" = "#726AA8"
  )
  component_stack_order <- c(
    "Pension",
    "Health",
    "Occupational Risk",
    "Bonuses and Benefits",
    "Payroll Taxes"
  )
  component_legend_order <- c(
    "Bonuses and Benefits",
    "Pension",
    "Health",
    "Occupational Risk",
    "Payroll Taxes"
  )
  country_name_map <- c(
    ARG = "Argentina",
    BOL = "Bolivia",
    BRA = "Brazil",
    CHL = "Chile",
    COL = "Colombia",
    CRI = "Costa Rica",
    DOM = "Dominican Republic",
    ECU = "Ecuador",
    ESP = "Spain",
    SLV = "El Salvador",
    GTM = "Guatemala",
    HND = "Honduras",
    MEX = "Mexico",
    NIC = "Nicaragua",
    PAN = "Panama",
    PRY = "Paraguay",
    PER = "Peru",
    URY = "Uruguay",
    US = "United States",
    VEN = "Venezuela"
  )
  country_display_name <- function(country_code) {
    if (is.null(country_code) || country_code == "") {
      return(country_code)
    }
    code <- toupper(country_code)
    mapped <- country_name_map[[code]]
    if (!is.null(mapped)) {
      return(mapped)
    }
    country_code
  }
  format_wage_label <- function(wage_code) {
    if (is.null(wage_code) || length(wage_code) == 0) {
      return(character(0))
    }
    wage_code <- wage_code[!is.na(wage_code)]
    if (length(wage_code) == 0) {
      return(character(0))
    }
    paste0(substr(wage_code, 1, nchar(wage_code) - 2), " MW")
  }
  format_wage_phrase <- function(wage_code) {
    if (is.null(wage_code) || length(wage_code) == 0) {
      return("selected minimum wage levels")
    }
    wage_code <- wage_code[!is.na(wage_code)]
    if (length(wage_code) == 0) {
      return("selected minimum wage levels")
    }
    if (length(wage_code) > 1) {
      return("selected minimum wage levels")
    }
    wage_value <- suppressWarnings(as.integer(sub("sm", "", wage_code)))
    wage_word <- switch(
      as.character(wage_value),
      "1" = "one",
      "2" = "two",
      "5" = "five",
      "10" = "ten",
      "15" = "fifteen",
      as.character(wage_value)
    )
    if (is.na(wage_value)) {
      return(format_wage_label(wage_code))
    }
    if (wage_value == 1) {
      return(paste(wage_word, "minimum wage"))
    }
    paste(wage_word, "minimum wages")
  }
  format_country_phrase <- function(countries) {
    if (is.null(countries) || length(countries) == 0 || "All" %in% countries) {
      return("across countries")
    }
    if (length(countries) == 1) {
      return(paste0("in ", country_display_name(countries[1])))
    }
    "across selected countries"
  }
  plot_title_text <- function() {
    subject <- switch(
      selected_group0(),
      all = "Non-salary labor costs",
      bonuses_and_benefits = "Bonuses and benefits",
      social = switch(
        selected_groupE(),
        pensions = "Pension contributions",
        health = "Health contributions",
        occupational_risk = "Occupational risk contributions",
        "Social security contributions"
      ),
      payroll_taxes = "Payroll taxes",
      "Non-salary labor costs"
    )

    if (selected_group0() == "bonuses_and_benefits" && selected_groupA() == "component") {
      subject <- switch(
        selected_groupD(),
        all_bonuses = "Bonuses and benefits",
        ab = "Annual and other bonuses",
        pl = "Paid leave",
        up = "Unemployment protection",
        ob = "Other bonuses and benefits",
        subject
      )
    }

    view_phrase <- ""
    if (selected_groupA() == "payer") {
      view_phrase <- " by payer"
    } else if (selected_groupA() == "component") {
      if (selected_group0() == "all") {
        view_phrase <- " by component"
      } else if (selected_group0() == "bonuses_and_benefits" && selected_groupD() == "all_bonuses") {
        view_phrase <- " by component"
      }
    }

    country_phrase <- format_country_phrase(ns_variables$country_sel)
    wage_phrase <- format_wage_phrase(selected_groupB())

    paste0(
      subject,
      view_phrase,
      " ",
      country_phrase,
      " as a percentage of ",
      wage_phrase,
      " (%)"
    )
  }
  y_axis_title_text <- function() {
    group0 <- selected_group0()

    if (group0 == "bonuses_and_benefits") {
      return("Bonuses and benefits as share of wages (%)")
    }
    if (group0 == "social") {
      groupE <- selected_groupE()
      if (groupE == "pensions") {
        return("Pension contribution as share of wages (%)")
      }
      if (groupE == "health") {
        return("Health contribution as share of wages (%)")
      }
      if (groupE == "occupational_risk") {
        return("Occupational risk as share of wages (%)")
      }
      return("Social security contributions as share of wages (%)")
    }
    if (group0 == "payroll_taxes") {
      return("Payroll taxes as share of wages (%)")
    }
    "Non-salary costs as share of wages (%)"
    }
  plot_footer_annotations <- function() {
    access_date <- format(Sys.Date(), "%Y-%m-%d")
    list(
      list(
        text = "Note: TBD",
        xref = "paper",
        yref = "paper",
        x = 0,
        y = -0.26,
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      ),
      list(
        text = paste0(
          "Source: World Bank, Regulatory Frameworks Database, 2026. Access date: ",
          access_date
        ),
        xref = "paper",
        yref = "paper",
        x = 0,
        y = -0.34,
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      )
    )
  }
  apply_plot_font <- function(fig) {
    fig %>% layout(
      font = list(family = plotly_font_family),
      title = list(
        text = plot_title_text(),
        x = 0.5,
        xanchor = "center"
      ),
      annotations = plot_footer_annotations(),
      margin = list(t = 60, b = 155)
    )
  }
  
  
  output$country_selection <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }
    choices <- ns_variables$countries
    selected_country <- "All"
    options <- NULL
    if (identical(mode, "wage")) {
      choices <- ns_variables$countries[ns_variables$countries != "All"]
      preferred <- "ARG"
      if (!preferred %in% choices) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% choices) {
        preferred <- choices[1]
      }
      selected_country <- preferred
      options <- list(maxItems = 1)
    }
    div(
      class = "pretty-select",
      selectizeInput(
        inputId = ns("country_selection_user"),
        label = "Country Analysis by:",
        choices = choices,
        selected = selected_country,
        multiple = TRUE,
        options = options
      )
    )
  })

  output$mw_selection_ui <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }
    allow_multiple <- identical(mode, "wage")
    selection <- selected_groupB()
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- "1sm"
    }
    if (!allow_multiple && length(selection) > 1) {
      selection <- selection[1]
    }

    div(
      class = "pretty-select mw-select",
      selectizeInput(
        inputId = ns("mw_selection"),
        label = NULL,
        choices = wage_choices,
        selected = selection,
        multiple = allow_multiple,
        options = list(plugins = list("remove_button"))
      )
    )
  })
  
  
  # ---- First Selection ----
  observeEvent(input$btn_total,  { selected_groupA("total") })
  observeEvent(input$btn_payer,  { selected_groupA("payer") })
  observeEvent(input$btn_component,  { 
    selected_groupA("component") 
  })
  observeEvent(input$all,  {
    selected_group0("all")
    selected_groupC("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$country_selection_user, {
    selection <- input$country_selection_user
    if (is.null(selection) || length(selection) == 0) {
      selection <- "All"
    }

    previous <- last_country_selection()
    if ("All" %in% selection && length(selection) > 1) {
      if (!("All" %in% previous)) {
        selection <- "All"
      } else {
        selection <- setdiff(selection, "All")
      }
    }

    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }
    if (identical(mode, "wage")) {
      if (is.null(selection) || length(selection) == 0 || "All" %in% selection) {
        preferred <- last_single_country()
        if (is.null(preferred) || preferred == "") {
          preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
        }
        selection <- preferred
      } else if (length(selection) > 1) {
        selection <- selection[1]
      }
    }

    if (!identical(selection, input$country_selection_user)) {
      updateSelectizeInput(session, ns("country_selection_user"), selected = selection)
    }

    ns_variables$country_sel <- selection
    last_country_selection(selection)
    if (length(selection) == 1 && selection != "All") {
      last_single_country(selection)
    }
  })

  option2_choices_for_group <- function(group0) {
    switch(
      group0,
      all = c("total", "payer", "component"),
      bonuses_and_benefits = c("total", "component"),
      social = c("component"),
      payroll_taxes = c("total"),
      c("total")
    )
  }

  observeEvent(selected_group0(), {
    valid_choices <- option2_choices_for_group(selected_group0())
    if (!selected_groupA() %in% valid_choices) {
      selected_groupA(valid_choices[1])
    }
  })

  observeEvent(input$compare_mode, {
    mode <- input$compare_mode
    if (is.null(mode)) {
      return()
    }
    previous_mode <- last_compare_mode()
    if (!is.null(previous_mode) && previous_mode == "wage" && mode == "country") {
      reset_across_defaults()
      return()
    }
    if (identical(mode, "wage")) {
      preferred <- "ARG"
      if (!preferred %in% ns_variables$countries) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% ns_variables$countries) {
        preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
      }
      updateSelectizeInput(session, ns("country_selection_user"), selected = preferred)
      updateSelectizeInput(session, ns("mw_selection"), selected = wage_levels)
      selected_groupB(wage_levels)
      last_wage_selection(wage_levels)
      if (!is.null(preferred) && preferred != "") {
        last_single_country(preferred)
      }
    } else {
      wages <- selected_groupB()
      if (length(wages) > 1) {
        wages <- wages[1]
        updateSelectizeInput(session, ns("mw_selection"), selected = wages)
        selected_groupB(wages)
        last_wage_selection(wages)
      }
      if (!is.null(ns_variables$order_country) &&
          length(ns_variables$order_country) > 0 &&
          any(grepl("\\bMW\\b", ns_variables$order_country))) {
        ns_variables$order_country <- NULL
      }
    }
    last_compare_mode(mode)
  })
  
  
  # ---- MW Selection ----
  observeEvent(input$mw_selection, {
    selection <- input$mw_selection
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- unique(selection)
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- last_wage_selection()
    }

    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }

    if (identical(mode, "country") && length(selection) > 1) {
      selection <- selection[1]
      showNotification("Across-country comparisons use one wage level.", type = "message")
    }

    if (identical(mode, "wage")) {
      countries <- ns_variables$country_sel
      if (is.null(countries) || length(countries) != 1 || "All" %in% countries) {
        selection <- last_wage_selection()
      }
    }

    if (!identical(selection, input$mw_selection)) {
      updateSelectizeInput(session, ns("mw_selection"), selected = selection)
    }

    selected_groupB(selection)
    last_wage_selection(selection)
  })
  
  # observeEvent(input$btn_sm1,  { selected_groupB("1sm") })
  # observeEvent(input$btn_sm2,  { selected_groupB("2sm") })
  # observeEvent(input$btn_sm5,  { selected_groupB("5sm") })
  # observeEvent(input$btn_sm10, { selected_groupB("10sm") })
  # observeEvent(input$btn_sm15, { selected_groupB("15sm") })
  
  # ---- Components ----
  observeEvent(input$all_component,  { selected_groupC("all_component") })
  observeEvent(input$bonus,  { 
    selected_groupC("bonuses_and_benefits")
    selected_group0("bonuses_and_benefits")
    option1_selected(TRUE)
  })
  observeEvent(input$social,  { 
    selected_group0("social")
    selected_groupC("social")
    option1_selected(TRUE)
  })
  observeEvent(input$payroll, {
    selected_group0("payroll_taxes")
    selected_groupC("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$pensions,  { selected_groupE("pensions") })
  observeEvent(input$health, { selected_groupE("health") })
  observeEvent(input$occupational_risk, { selected_groupE("occupational_risk") })
  
  # ---- Bonuses and Benefits ----
  observeEvent(input$ab,  { selected_groupD("ab") })
  observeEvent(input$pl,  { selected_groupD("pl") })
  observeEvent(input$ob,  { selected_groupD("ob") })
  observeEvent(input$up,  { selected_groupD("up") })
  
  # ---- Graph ----
  output$plot <- renderPlotly({
    
    # Requirements
    req(selected_groupA())
    req(selected_groupB())
    
    # Results from user click
    group0 <- selected_group0()
    groupA <- selected_groupA()
    groupB <- selected_groupB()
    groupC <- selected_groupC()
    groupD <- selected_groupD()
    groupE <- selected_groupE()
    
    wage_codes <- groupB
    if (is.null(wage_codes) || length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- as.character(wage_codes)
    wage_codes <- wage_codes[!is.na(wage_codes)]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- wage_levels[wage_levels %in% wage_codes]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    compare_wages <- identical(input$compare_mode, "wage") && length(wage_codes) > 1
    if (!compare_wages && length(wage_codes) > 1) {
      wage_codes <- wage_codes[1]
    }
    # Transform values from "1sm" → "1 MW"
    wage_filter <- format_wage_label(wage_codes)
    panel_order <- function() {
      if (compare_wages) {
        return(wage_filter)
      }
      current <- ns_variables$order_country
      if (is.null(current) || length(current) == 0) {
        return(NULL)
      }
      if (any(grepl("\\bMW\\b", current))) {
        return(NULL)
      }
      current
    }
    if (compare_wages) {
      ns_variables$order_country <- wage_filter
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }
    }
    y_axis_title <- y_axis_title_text()

    apply_wage_panels <- function(df) {
      if (!compare_wages || !"wage" %in% names(df)) {
        return(df)
      }
      df$country <- df$wage
      df
    }
    build_multicategory_stack <- function(df, colors, y_axis_title,
                                          stack_order = names(colors),
                                          legend_order = names(colors),
                                          legend_traceorder = "normal") {
      df <- df %>%
        filter(!is.na(Type), !is.na(value)) %>%
        mutate(
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          wage = factor(wage, levels = wage_filter),
          Type = factor(Type, levels = stack_order)
        ) %>%
        arrange(Scenario, wage)

      fig <- plot_ly(type = "bar")
      for (type in levels(df$Type)) {
        sub <- df %>% filter(Type == type)
        if (nrow(sub) == 0) {
          next
        }
        fig <- fig %>% add_trace(
          x = list(sub$Scenario, sub$wage),
          y = sub$value,
          name = type,
          marker = list(color = colors[[type]]),
          hoverinfo = "y+name",
          legendrank = match(type, legend_order)
        )
      }

      fig <- fig %>%
        layout(
          barmode = "stack",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.2,
            traceorder = legend_traceorder
          )
        )

      fig <- apply_plot_font(fig)
      fig <- fig %>% layout(annotations = plot_footer_annotations())
      fig
    }
    build_component_subplot <- function(df, show_legend, y_axis_title, legend_order = component_legend_order) {
      fig <- plot_ly(type = "bar")
      if (show_legend) {
        for (type in legend_order) {
          fig <- fig %>% add_trace(
            x = NA,
            y = NA,
            name = type,
            marker = list(color = component_palette[[type]]),
            showlegend = TRUE,
            legendgroup = type,
            hoverinfo = "none",
            visible = "legendonly"
          )
        }
      }
      for (type in component_stack_order) {
        sub <- df %>% filter(Type == type)
        if (nrow(sub) == 0) {
          next
        }
        fig <- fig %>% add_trace(
          x = ~Scenario,
          y = ~value,
          data = sub,
          name = type,
          marker = list(color = component_palette[[type]]),
          showlegend = FALSE,
          legendgroup = type,
          hoverinfo = "y+name"
        )
      }
      fig %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE,
            tickangle = 90
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          barmode = "stack"
        )
    }

    if (compare_wages && groupA == "total") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      if (group0 == "all") {
        df <- df_non_salary %>%
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          mutate(
            Scenario = ifelse(type == "t_min", "Min", "Max")
          ) %>%
          select(wage, Scenario, value)
      } else {
        path_component <- paste0("data/non_salary/", paste0(group0, "_all.rds"))
        df <- readRDS(path_component) %>%
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          ) %>%
          select(wage, Scenario, value)
      }

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df %>%
        mutate(
          wage = factor(wage, levels = wage_filter),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) %>%
        arrange(Scenario, wage)

      fig <- plot_ly(
        data = df,
        x = list(df$Scenario, df$wage),
        y = ~value,
        type = "bar",
        marker = list(color = "#002244"),
        showlegend = FALSE
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )

      fig <- apply_plot_font(fig)
      fig <- fig %>% layout(annotations = plot_footer_annotations())
      return(fig)
    }

    if (compare_wages && groupA == "component" && group0 == "all") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- df_non_salary_component %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        mutate(
          Scenario = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
          Type = ifelse(grepl("_pension", type_by_component), "Pension",
                        ifelse(grepl("_health", type_by_component), "Health",
                               ifelse(grepl("_bonuses", type_by_component), "Bonuses and Benefits",
                                      ifelse(grepl("_occupational", type_by_component), "Occupational Risk",
                                             "Payroll Taxes"))))
        ) %>%
        select(wage, Scenario, Type, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      return(build_multicategory_stack(
        df,
        component_palette,
        y_axis_title,
        stack_order = component_stack_order,
        legend_order = component_legend_order
      ))
    }

    if (compare_wages && groupA == "component" && group0 == "bonuses_and_benefits") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      path_component <- paste0("data/non_salary/", paste0(group0, "_component.rds"))
      df <- readRDS(path_component) %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
          Type = ifelse(component == "ab", "Annual and other periodic bonuses",
                        ifelse(component == "pl", "Paid Leave",
                               ifelse(component == "up", "Unemployment Protection",
                                      ifelse(component == "ob", "Other bonuses", NA))))
        ) %>%
        select(wage, Scenario, Type, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      colors <- c(
        "Annual and other periodic bonuses" = "#002244",
        "Paid Leave" = "#8EA2BF",
        "Unemployment Protection" = "#B9BAB5",
        "Other bonuses" = "#6F6779"
      )
      return(build_multicategory_stack(df, colors, y_axis_title))
    }
    
    
    # ---- ALL and Total ----
    
    if (group0=="all" & groupA == "total" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        
        # Filtering total non salary
        df <- df_non_salary %>%
          dplyr::filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df %>%
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) %>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
      }
      
      else{
        df <- df_non_salary %>%
          filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df %>%
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) %>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   # 🔥 CLAVE
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
      }
      
    }
    
    if (group0=="all" & groupA == "total" & length(ns_variables$country_sel)>1) {
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      ns_variables$countries=c("All",unique(df_non_salary$country))
      # Filtering total non salary
      df <- df_non_salary %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) %>%
        apply_wage_panels()
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide <- df %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value
        ) %>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      ns_variables$order_country <- unique(as.character(df_wide$country))
      
      df_mm <- df_wide %>%
        tidyr::pivot_longer(
          cols = c(t_min, t_max),
          names_to = "Scenario",
          values_to = "value"
        ) %>%
        mutate(
          Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm %>% filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = c("Min" = "#00C1FF", "Max" = "#002244"),
          showlegend = FALSE
        ) %>%
          layout(
            barmode = "stack",   # 🔥 CLAVE
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) %>%
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_plot_font(fig))
    }
    
    # ---- ALL By Payer ----
    
    if (group0=="all" & groupA == "payer" & length(ns_variables$country_sel)==1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_payer %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels() %>%
          select(country, type_by_payer, value) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
            payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
      }
      
      else{
        
        df_long <- df_non_salary_payer %>%
          filter(
            wage %in% wage_filter,
            country== ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, type_by_payer, value) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
            payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
      }
      
    }
    
    if (group0=="all" & groupA == "payer" & length(ns_variables$country_sel)>1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      
      df_long <- df_non_salary_payer %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) %>%
        apply_wage_panels() %>%
        select(country, type_by_payer, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
          payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      df_long <- df_long %>%
        mutate(country = factor(country, levels = panel_order())) %>% 
        arrange(country)
      
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) %>%
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_plot_font(fig))
    }
    
    # ---- ALL by Component ----

    if (group0=="all" & groupA == "component" & length(ns_variables$country_sel)==1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_component %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels() %>%
          select(country, type_by_component, value) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
            payer = ifelse(grepl("_pension", type_by_component), "Pension", 
                           ifelse(grepl("_health", type_by_component), "Health",
                                  ifelse(grepl("_bonuses", type_by_component), "Bonuses and Benefits",
                                         ifelse(grepl("_occupational", type_by_component), "Occupational Risk","Payroll Taxes")))),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order
          ) %>%
            layout(
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              )
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
      }
      else{
        df_long <- df_non_salary_component %>%
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, type_by_component, value) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
            payer = ifelse(grepl("^st_p", type_by_component), "Pension", 
                           ifelse(grepl("^st_h", type_by_component), "Health",
                                  ifelse(grepl("^st_b", type_by_component), "Bonuses and Benefits",
                                         ifelse(grepl("^st_or", type_by_component), "Occupational Risk","Payroll Taxes")))),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order
          ) %>%
            layout(
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              )
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
      }
    }
    
    if (group0=="all" & groupA == "component" & length(ns_variables$country_sel)>1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      df_long <- df_non_salary_component %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel 
        ) %>%
        apply_wage_panels() %>%
        select(country, type_by_component, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
          payer = ifelse(grepl("^st_p", type_by_component), "Pension", 
                         ifelse(grepl("^st_h", type_by_component), "Health",
                                ifelse(grepl("^st_b", type_by_component), "Bonuses and Benefits",
                                       ifelse(grepl("^st_or", type_by_component), "Occupational Risk","Payroll Taxes")))),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = component_stack_order)
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- build_component_subplot(
          data_pais,
          show_legend = show_legend,
          y_axis_title = ifelse(i == 1, y_axis_title, ""),
          legend_order = component_legend_order
        ) %>%
          layout(
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) %>%
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_plot_font(fig))
    }
    
    # ---- bonuses and benefits/Payroll and Total ----
    
    if ((group0!="all" | group0!="social") & groupA == "total" & length(ns_variables$country_sel)==1) {
    
    if(ns_variables$country_sel=="All"){
        path_component=paste0("data/non_salary/",paste0(group0,"_all.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        ns_variables$countries=c("All",unique(df$country))
      
    
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        #ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
    }
    else{
        path_component=paste0("data/non_salary/",paste0(group0,"_all.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
      
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        #ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
    }
    }
    
    if((group0!="all" | group0!="social") & groupA == "total" & length(ns_variables$country_sel)>1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
        path_component=paste0("data/non_salary/",paste0(group0,"_all.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
          
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        #ns_variables$order_country <- unique(as.character(df_wide$country))
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
    }
    
    # ---- bonuses and benefits and Components ----
    
    if ((group0=="bonuses_and_benefits") & groupA == "component" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        
        path_component=paste0("data/non_salary/",paste0(group0,"_component.rds"))
        df=readRDS(path_component)
        df_long <- df  %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_component,component, value) %>%
          mutate(
            group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
            payer = ifelse(component=="ab", "Annual and other periodic bonuses", 
                           ifelse(component=="pl", "Paid Leave",
                                  ifelse(component=="up", "Unemployment Protection",
                                         ifelse(component=="ob", "Other bonuses",NA)))),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Annual and other periodic bonuses","Paid Leave", 
                                               "Unemployment Protection","Other bonuses"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Annual and other periodic bonuses"="#002244","Paid Leave"="#8EA2BF",
                    "Unemployment Protection"="#B9BAB5","Other bonuses"="#6F6779")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
       
      }
      else{
        path_component=paste0("data/non_salary/",paste0(group0,"_component.rds"))
        df=readRDS(path_component)
        df_long <- df  %>%
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_component,component, value) %>%
          mutate(
            group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
            payer = ifelse(component=="ab", "Annual and other periodic bonuses", 
                           ifelse(component=="pl", "Paid Leave",
                                  ifelse(component=="up", "Unemployment Protection",
                                         ifelse(component=="ob", "Other bonuses",NA)))),
            group = factor(group, levels = c("Min", "Max"))
          )
        
        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        if (nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Annual and other periodic bonuses","Paid Leave", 
                                               "Unemployment Protection","Other bonuses"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Annual and other periodic bonuses"="#002244","Paid Leave"="#8EA2BF",
                    "Unemployment Protection"="#B9BAB5","Other bonuses"="#6F6779")
        
        paises <- unique(df$country)
        
        
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 5
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.15
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_plot_font(fig))
      }
      
    }
    if (groupA == "component" & groupC!="all_component" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
          path_component=paste0("data/non_salary/",paste0(groupC,"_component.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage %in% wage_filter,
              component == groupD
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_component, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        }
        if(groupC=="social"){
          path_component=paste0("data/non_salary/",paste0(groupE,"_all.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage %in% wage_filter
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        }
        else{
          if(input$component_type=="Total"){
            path_component=paste0("data/non_salary/",paste0(groupC,"_all.rds"))
            df=readRDS(path_component)
            df <- df %>%
              filter(
                wage %in% wage_filter
              ) %>%
              apply_wage_panels() %>%
              select(country, min_max_total, value) %>%
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
            ns_variables$countries=c("All",unique(df$country))
          }
        }
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        #ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
      }
      else{
        if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
          path_component=paste0("data/non_salary/",paste0(groupC,"_component.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage %in% wage_filter,
              component == groupD,
              country==ns_variables$country_sel
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_component, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
        }
        if(groupC=="social"){
          path_component=paste0("data/non_salary/",paste0(groupE,"_all.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage %in% wage_filter,
              country==ns_variables$country_sel
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
        }
        else{
          if(input$component_type=="Total"){
            path_component=paste0("data/non_salary/",paste0(groupC,"_all.rds"))
            df=readRDS(path_component)
            df <- df %>%
              filter(
                wage %in% wage_filter,
                country==ns_variables$country_sel
              ) %>%
              apply_wage_panels() %>%
              select(country, min_max_total, value) %>%
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
          }
        }
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        #ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_plot_font(fig))
      }
      
    }
    if (groupA == "component" & groupC!="all_component" & length(ns_variables$country_sel)>1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
        path_component=paste0("data/non_salary/",paste0(groupC,"_component.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage %in% wage_filter,
            component == groupD,
            country %in% ns_variables$country_sel 
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      }
      if(groupC=="social"){
        path_component=paste0("data/non_salary/",paste0(groupE,"_component.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage %in% wage_filter,
            component == groupD,
            country %in% ns_variables$country_sel 
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      }
      else{
        if(input$component_type=="Total"){
          path_component=paste0("data/non_salary/",paste0(groupC,"_all.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage %in% wage_filter,
              country %in% ns_variables$country_sel
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
        }
      }
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df_wide=df %>%
        group_by(country) %>%
        summarize(
          t_min = min(value, na.rm = TRUE),
          t_max = max(value, na.rm = TRUE)
        )%>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      #ns_variables$order_country <- unique(as.character(df_wide$country))
      df_mm <- df_wide %>%
        tidyr::pivot_longer(
          cols = c(t_min, t_max),
          names_to = "Scenario",
          values_to = "value"
        ) %>%
        mutate(
          Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm %>% filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = c("Min" = "#00C1FF", "Max" = "#002244"),
          showlegend = FALSE
        ) %>%
          layout(
            barmode = "stack",   
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) %>%
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_plot_font(fig))
    }
    
  })
  
  
  output$tabla_detalle<-reactable::renderReactable({
    table_visible(FALSE)
    ns_variables$df_final_tabla <- NULL

    groupA <- selected_groupA()
    groupC <- selected_groupC()
    groupD <- selected_groupD()
    groupE <- selected_groupE()
    
    con_sel=ns_variables$country_sel
    if(groupA!= "component" ) return()
    else{
      data <- NULL
      if(groupA== "component" & groupC=="all_component"){
        return()
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="all_bonuses"){
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL All B")
        data <- as.data.frame(data)
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
        
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="ab"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL ab")
        data <- as.data.frame(data)
        
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="pl"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL pl")
        data <- as.data.frame(data)
        
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="up"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL up")
        data <- as.data.frame(data)
        
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="ob"){
        print("estoy aca")
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL Or")
        data <- as.data.frame(data)
        
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupE=="health"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL H")
        data <- as.data.frame(data)
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupE=="payroll_taxes"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL Pt")
        data <- as.data.frame(data)
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
      else if (groupA== "component" & groupE=="pensions"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL All P")
        data <- as.data.frame(data)
        if(!"All" %in% con_sel){
          data=data %>% dplyr::filter(Country %in% con_sel)
        }
        ns_variables$df_final_tabla=data
      }
    
    if (is.null(data)) {
      return()
    }
    table_visible(TRUE)
    
    reactable::reactable(
      data,
      
      # Estilo general aplicado a todas las columnas
      defaultColDef = reactable::colDef(
        html = TRUE,
        minWidth = 140,
        maxWidth = 260,
        align = "left",
        style = list(
          whiteSpace = "normal",     # permite texto multilínea
          lineHeight = "1.35",
          fontSize = "12px",
          padding = "6px",
          textAlign = "justify",
          fontFamily = plotly_font_family
        )
      ),
      theme = reactable::reactableTheme(
        style = list(fontFamily = plotly_font_family),
        headerStyle = list(fontFamily = plotly_font_family)
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      defaultPageSize = 8
    )
    
} 
    
  })
  
  output$option2_buttons <- renderUI({
    if (!option1_selected()) {
      return(div(style = "display:none;"))
    }

    group0 <- selected_group0()
    if (group0 == "payroll_taxes") {
      return(div(style = "display:none;"))
    }
    valid_choices <- option2_choices_for_group(group0)
    button_style <- paste(
      "background-color: #e6f4ff;",
      "color: #0f3b66;",
      "border: 1px solid #0f3b66;",
      "border-radius: 20px;",
      "padding: 6px 18px;",
      "font-weight: 600;"
    )

    option_button <- function(id, label, value, title) {
      btn_class <- if (identical(selected_groupA(), value)) {
        "pill-button subcomponent-btn active"
      } else {
        "pill-button subcomponent-btn"
      }

      tags$div(
        style = "display: flex; flex-direction: column; gap: 4px;",
        actionButton(ns(id), label, class = btn_class, title = title, style = button_style)
      )
    }

    tags$div(
      class = "option2-group",
      style = "display: flex; flex-direction: column; gap: 8px;",
      tags$span("Explore by subcomponents:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
      if ("total" %in% valid_choices) option_button("btn_total", "TOTAL", "total", "Show total non-salary costs."),
      if ("payer" %in% valid_choices) option_button("btn_payer", "BY PAYER", "payer", "Split costs by payer (employer vs. employee)."),
      if ("component" %in% valid_choices && group0 != "social") {
        option_button("btn_component", "BY COMPONENT", "component", "Break down costs by component.")
      }
    )
  })
  
  
  # --- Components ----
  output$component_buttons <- renderUI({
    group0 <- selected_group0()
    groupA <- selected_groupA()
    
    if (group0 != "social") {
      return(div(style="visibility:hidden;"))
    }
    else if (group0 == "social" & groupA =="component"){
      button_class <- function(value) {
        if (identical(selected_groupE(), value)) {
          "component-btn active"
        } else {
          "component-btn"
        }
      }
      
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:flex-start; justify-content:space-between; width:100%;",
        
        # ---- titulo ----
        div(
          tags$div(
            "Social Security Contributions Components",
            style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
          )
        ),
        
        # ---- botones a la izquierda ----
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
          actionButton(
            ns("pensions"),
            "Pension",
            class = button_class("pensions")
          ),
          
          actionButton(
            ns("health"),
            "Health",
            class = button_class("health")
          ),
          
          actionButton(
            ns("occupational_risk"),
            "Occupational Risk",
            class = button_class("occupational_risk")
          )
        )
      )
      # div(
      #   class = "horizontal-container",
      #   style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
      #   
      #   # ---- botones a la izquierda ----
      #   div(
      #     class = "component-buttons-container",
      #     style = "display:flex; flex-wrap:wrap; gap:8px;",
      #     
      #     actionButton(
      #       ns("all_component"),
      #       "All",
      #       class = "component-btn active"
      #     ),
      #     
      #     actionButton(
      #       ns("bonus"),
      #       "Bonuses and Benefits",
      #       class = "component-btn"
      #     ),
      #     
      #     actionButton(
      #       ns("social"),
      #       "Social Security Contributions",
      #       class = "component-btn"
      #     ),
      #     
      #     
      #     actionButton(
      #       ns("payroll"),
      #       "Payroll Taxes",
      #       class = "component-btn"
      #     )
      #   )
      # )
    }
  })
  
  output$bonus_buttons <- renderUI({
    group0 <- selected_group0()
    groupC <- selected_groupC()
    groupA <- selected_groupA()
    
    if(group0 != "bonuses_and_benefits"){
      return(div(style="visibility:hidden;"))
    }
    else if (groupC == "bonuses_and_benefits" & groupA =="component") {
      bonus_class <- function(value) {
        if (identical(selected_groupD(), value)) {
          "component-btn active"
        } else {
          "component-btn"
        }
      }
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:flex-start; justify-content:space-between; width:100%;",
        
        # ---- título ----
        div(
          tags$div(
            "Bonuses and Benefits Components",
            style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
          )
        ),
        
        # ---- botones a la izquierda ----
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
          
          actionButton(
            ns("all_bonuses"),
            "All Bonuses",
            class = bonus_class("all_bonuses")
          ),
          
          actionButton(
            ns("ab"),
            "Annual and other bonuses",
            class = bonus_class("ab")
          ),
          
          actionButton(
            ns("pl"),
            "Paid Leave",
            class = bonus_class("pl")
          ),
          
          actionButton(
            ns("up"),
            "Unemployment Protection",
            class = bonus_class("up")
          ),
          
          actionButton(
            ns("ob"),
            "other bonuses and benefits",
            class = bonus_class("ob")
          )
        )
      )
    }
    
    # if ((groupC != "bonuses_and_benefits" | groupC != "social") & groupA !="component") {
    #   return(div(style="visibility:hidden;"))
    # }
    
    # else if (groupC == "bonuses_and_benefits" & groupA =="component") {
    #   div(
    #     class = "horizontal-container",
    #     style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
    #     
    #     # ---- título ----
    #     div(
    #       tags$div(
    #         "Bonuses and Benefits Components",
    #         style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
    #       )
    #     ),
    #     
    #     # ---- botones a la izquierda ----
    #     div(
    #       class = "component-buttons-container",
    #       style = "display:flex; flex-wrap:wrap; gap:8px;",
    #       
    #       actionButton(
    #         ns("all_bonuses"),
    #         "All Bonuses",
    #         class = "component-btn active"
    #       ),
    #       
    #       actionButton(
    #         ns("ab"),
    #         "Annual and other bonuses",
    #         class = "component-btn"
    #       ),
    #       
    #       actionButton(
    #         ns("pl"),
    #         "Paid Leave",
    #         class = "component-btn"
    #       ),
    #       
    #       actionButton(
    #         ns("up"),
    #         "Unemployment Protection",
    #         class = "component-btn"
    #       ),
    #       
    #       actionButton(
    #         ns("ob"),
    #         "other bonuses and benefits",
    #         class = "component-btn"
    #       )
    #     )
    #   )
    # }
    # 
    # else if (groupC == "social" & groupA =="component"){
      # div(
      #   class = "horizontal-container",
      #   style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
      # 
      #   # ---- titulo ----
      #   div(
      #     tags$div(
      #       "Social Security Contributions",
      #       style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
      #     )
      #   ),
      # 
      #   # ---- botones a la izquierda ----
      #   div(
      #     class = "component-buttons-container",
      #     style = "display:flex; flex-wrap:wrap; gap:8px;",
      #     actionButton(
      #       ns("pensions"),
      #       "Pension",
      #       class = "component-btn"
      #     ),
      # 
      #     actionButton(
      #       ns("health"),
      #       "Health",
      #       class = "component-btn"
      #     ),
      # 
      #     actionButton(
      #       ns("occupational_risk"),
      #       "Occupational Risk",
      #       class = "component-btn"
      #     )
      #   )
      # )
    # }
    
  })
  
  
  
  output$download_df <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_table_ui <- renderUI({
    if (!table_visible()) {
      return(NULL)
    }
    downloadButton(
      outputId = ns("download_table"),
      label = "DOWNLOAD TABLE",
      style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
    )
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Legislation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final_tabla, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

