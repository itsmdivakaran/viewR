# =============================================================================
# ViewR -- server.R
# Full Shiny server: reactive data pipeline, dynamic filter/sort rows,
# column visibility, Excel-like editing, find-and-replace, code generation.
# =============================================================================


#' Build the ViewR Shiny server function.
#'
#' @param orig_data   original data.frame (never mutated)
#' @param data_name   character name of the source object
#' @param labels      named character vector of variable labels
#' @param edit_mode   logical
#' @param gen_code    logical
#' @param max_display integer - cap on rows shown in the view table
#' @noRd
.vr_server <- function(orig_data, data_name, labels, edit_mode,
                        gen_code, max_display) {

  all_cols <- names(orig_data)

  function(input, output, session) {

    # -- Central state ---------------------------------------------------------
    s <- shiny::reactiveValues(
      # Working data (accumulates edits and find/replace operations)
      data_work    = orig_data,

      # Dynamic filter / sort row counters and id lists
      filter_count = 0L,
      filter_ids   = integer(0),
      sort_count   = 0L,
      sort_ids     = integer(0),

      # Find-replace preview data frame
      fnr_preview  = NULL,

      # Accumulated find-replace mutate() code strings
      fnr_ops      = character(0),

      # Accumulated edit descriptions (free text)
      edit_ops     = character(0)
    )


    # ==========================================================================
    # REACTIVE DATA PIPELINE
    # active_filters() -> active_sorts() -> data_processed() -> data_displayed()
    # ==========================================================================

    # Read live filter inputs and assemble a list of filter specs
    active_filters <- shiny::reactive({
      fids <- s$filter_ids
      if (length(fids) == 0) return(list())

      specs <- lapply(fids, function(fid) {
        col <- input[[paste0("f_col_",   fid)]]
        op  <- input[[paste0("f_op_",    fid)]]
        val <- input[[paste0("f_val_",   fid)]] %||% ""
        lgc <- if (fid == min(fids)) "AND"
               else input[[paste0("f_logic_", fid)]] %||% "AND"
        if (is.null(col) || is.null(op)) return(NULL)
        list(id = fid, column = col, operator = op, value = val, logic = lgc)
      })
      Filter(Negate(is.null), specs)
    })

    # Read live sort inputs
    active_sorts <- shiny::reactive({
      sids <- s$sort_ids
      if (length(sids) == 0) return(list())

      specs <- lapply(sids, function(sid) {
        col <- input[[paste0("s_col_", sid)]]
        dir <- input[[paste0("s_dir_", sid)]] %||% "asc"
        if (is.null(col)) return(NULL)
        list(id = sid, column = col, direction = dir)
      })
      Filter(Negate(is.null), specs)
    })

    # Apply filters + sorts to working data
    data_processed <- shiny::reactive({
      d <- s$data_work
      d <- .vr_apply_filters(d, active_filters())
      d <- .vr_apply_sorts(d,   active_sorts())
      d
    })

    # Apply column visibility
    data_displayed <- shiny::reactive({
      d    <- data_processed()
      vcols <- input$vr_visible_cols
      if (is.null(vcols) || length(vcols) == 0) return(d)
      keep <- intersect(vcols, names(d))
      if (length(keep) == 0) return(d)
      d[, keep, drop = FALSE]
    })


    # ==========================================================================
    # STATUS BAR
    # ==========================================================================
    output$vr_status <- shiny::renderText({
      total    <- nrow(s$data_work)
      filtered <- nrow(data_processed())
      nf       <- length(active_filters())
      ns       <- length(active_sorts())
      vcols    <- input$vr_visible_cols %||% all_cols

      parts <- c(
        paste0(total, " rows \u00d7 ", length(vcols), " col",
               if (length(vcols) != 1) "s"),
        if (nf > 0) paste0("Filtered: ", filtered, " row",
                           if (filtered != 1) "s"),
        if (ns > 0) paste0("Sorted by: ",
                           paste(vapply(active_sorts(), `[[`, character(1), "column"),
                                 collapse = ", "))
      )
      paste(Filter(Negate(is.null), parts), collapse = "  \u2502  ")
    })


    # ==========================================================================
    # CONDITIONAL PANEL TRIGGERS
    # ==========================================================================
    output$vr_has_filters <- shiny::reactive(length(s$filter_ids) > 0)
    output$vr_has_sorts   <- shiny::reactive(length(s$sort_ids)   > 0)
    shiny::outputOptions(output, "vr_has_filters", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "vr_has_sorts",   suspendWhenHidden = FALSE)

    output$vr_fnr_has_preview <- shiny::reactive(
      !is.null(s$fnr_preview) && nrow(s$fnr_preview) > 0
    )
    shiny::outputOptions(output, "vr_fnr_has_preview",
                         suspendWhenHidden = FALSE)


    # ==========================================================================
    # MAIN DATA TABLE (DT)
    # ==========================================================================
    output$vr_table <- DT::renderDataTable({
      d    <- data_displayed()
      pg   <- input$vr_page_length %||% 25
      rn   <- isTRUE(input$vr_show_rownames)
      lbl  <- isTRUE(input$vr_show_labels)

      # Cap rows for performance
      shown <- if (nrow(d) > max_display) {
        shiny::showNotification(
          paste0("Showing first ", max_display,
                 " of ", nrow(d), " rows for performance."),
          type = "warning", duration = 4, id = "vr_cap_notice"
        )
        d[seq_len(max_display), , drop = FALSE]
      } else d

      # Build custom container for tooltip headers when labels are active.
      # Using container (not colnames) avoids the DT escape/filter conflict
      # that occurs when HTML strings are passed through colnames with filter="top".
      container <- if (lbl) {
        cells <- lapply(names(shown), function(col) {
          lb <- labels[[col]]
          if (!is.null(lb) && !is.na(lb) && nzchar(lb))
            htmltools::tags$th(
              htmltools::tags$span(
                title = lb,
                style = "cursor:help; border-bottom:1px dotted currentColor;",
                col, htmltools::tags$sup("\u2139")
              )
            )
          else
            htmltools::tags$th(col)
        })
        if (rn) cells <- c(list(htmltools::tags$th("")), cells)
        htmltools::withTags(
          htmltools::tags$table(
            class = "display",
            htmltools::tags$thead(htmltools::tags$tr(cells))
          )
        )
      } else NULL

      DT::datatable(
        shown,
        rownames  = rn,
        container = container,
        escape    = TRUE,
        class     = "compact stripe hover cell-border",
        filter    = "top",
        selection = "multiple",
        options   = list(
          pageLength    = pg,
          scrollX       = TRUE,
          scrollY       = "460px",
          scrollCollapse = TRUE,
          dom           = "lrtip",
          columnDefs    = list(list(targets = "_all",
                                    className = "dt-middle")),
          language      = list(
            search     = "Search:",
            lengthMenu = "Show _MENU_ rows",
            info       = "_START_\u2013_END_ of _TOTAL_ rows"
          )
        )
      )
    }, server = TRUE)


    # ==========================================================================
    # RHANDSONTABLE  (edit mode)
    # ==========================================================================
    if (edit_mode) {

      # Undo stack
      undo_stack <- shiny::reactiveValues(history = list(), pointer = 0L)

      output$vr_hot <- rhandsontable::renderRHandsontable({
        d    <- data_processed()
        vcols <- intersect(input$vr_visible_cols %||% all_cols, names(d))
        if (length(vcols) == 0) vcols <- names(d)
        d_show <- d[, vcols, drop = FALSE]

        rhandsontable::rhandsontable(
          d_show,
          readOnly    = FALSE,
          useTypes    = TRUE,
          stretchH    = "all",
          contextMenu = TRUE,
          height      = 500
        ) |>
          rhandsontable::hot_table(
            highlightCol = TRUE,
            highlightRow = TRUE
          )
      })

      # Add a blank row
      shiny::observeEvent(input$vr_add_row, {
        new_row <- as.data.frame(lapply(s$data_work, function(col) {
          if      (is.integer(col))   NA_integer_
          else if (is.numeric(col))   NA_real_
          else if (is.logical(col))   NA
          else                        NA_character_
        }), stringsAsFactors = FALSE)
        undo_stack$history <- c(
          undo_stack$history[seq_len(undo_stack$pointer)],
          list(s$data_work)
        )
        undo_stack$pointer <- length(undo_stack$history)
        s$data_work <- rbind(s$data_work, new_row)
        s$edit_ops <- c(s$edit_ops, "Row added")
        shiny::showNotification("Row added.", type = "message", duration = 2)
      })

      # Delete selected rows
      shiny::observeEvent(input$vr_del_row, {
        shiny::req(input$vr_hot)
        hot_sel <- input$vr_hot$params$data
        # rhandsontable doesn't expose selected rows directly;
        # show guidance instead
        shiny::showNotification(
          "Select row(s) in the table (click row header), then use right-click \u2192 Remove row.",
          type = "message", duration = 5
        )
      })

      # Sync edits from handsontable back to s$data_work
      shiny::observeEvent(input$vr_hot, {
        shiny::req(input$vr_hot)
        hot_df <- tryCatch(
          rhandsontable::hot_to_r(input$vr_hot),
          error = function(e) NULL
        )
        if (is.null(hot_df)) return()

        vcols <- intersect(input$vr_visible_cols %||% all_cols, names(s$data_work))
        if (length(vcols) == 0) vcols <- names(s$data_work)

        # Only write back if dimensions match (guard against stale renders)
        proc <- data_processed()
        if (nrow(hot_df) == nrow(proc) && length(vcols) == ncol(hot_df)) {
          if (length(active_filters()) == 0 && length(active_sorts()) == 0) {
            new_vals <- hot_df[, seq_along(vcols), drop = FALSE]
            # Only push undo and update when the data actually changed
            if (!identical(s$data_work[, vcols, drop = FALSE], new_vals)) {
              undo_stack$history <- c(
                undo_stack$history[seq_len(undo_stack$pointer)],
                list(s$data_work)
              )
              undo_stack$pointer <- length(undo_stack$history)
              s$data_work[, vcols] <- new_vals
            }
          } else {
            shiny::showNotification(
              "Clear all filters and sorts before editing to ensure correct row mapping.",
              type = "warning", duration = 5, id = "vr_edit_warn"
            )
          }
        }
      }, ignoreInit = TRUE)

      # Undo
      shiny::observeEvent(input$vr_edit_undo, {
        if (undo_stack$pointer > 0) {
          s$data_work        <- undo_stack$history[[undo_stack$pointer]]
          undo_stack$pointer <- undo_stack$pointer - 1L
          shiny::showNotification("Undo applied.", type = "message", duration = 1)
        } else {
          shiny::showNotification("Nothing to undo.", type = "warning", duration = 2)
        }
      })

      # Redo
      shiny::observeEvent(input$vr_edit_redo, {
        if (undo_stack$pointer < length(undo_stack$history)) {
          undo_stack$pointer <- undo_stack$pointer + 1L
          s$data_work        <- undo_stack$history[[undo_stack$pointer]]
          shiny::showNotification("Redo applied.", type = "message", duration = 1)
        } else {
          shiny::showNotification("Nothing to redo.", type = "warning", duration = 2)
        }
      })

    }  # end edit_mode block


    # ==========================================================================
    # DYNAMIC FILTER ROWS
    # ==========================================================================

    shiny::observeEvent(input$vr_add_filter, {
      s$filter_count <- s$filter_count + 1L
      fid            <- s$filter_count
      is_first       <- length(s$filter_ids) == 0L
      s$filter_ids   <- c(s$filter_ids, fid)

      shiny::insertUI(
        selector = "#vr_filter_container",
        where    = "beforeEnd",
        ui       = .vr_filter_row_ui(fid, all_cols, is_first)
      )

      # One-time observer per remove button
      local({
        local_fid <- fid
        shiny::observeEvent(
          input[[paste0("f_remove_", local_fid)]],
          {
            shiny::removeUI(selector = paste0("#filter_row_", local_fid))
            s$filter_ids <- setdiff(s$filter_ids, local_fid)
          },
          ignoreInit = TRUE,
          once       = TRUE
        )
      })
    })

    shiny::observeEvent(input$vr_clear_filters, {
      for (fid in s$filter_ids) {
        shiny::removeUI(selector = paste0("#filter_row_", fid))
      }
      s$filter_ids <- integer(0)
    })


    # ==========================================================================
    # DYNAMIC SORT ROWS
    # ==========================================================================

    shiny::observeEvent(input$vr_add_sort, {
      s$sort_count <- s$sort_count + 1L
      sid          <- s$sort_count
      s$sort_ids   <- c(s$sort_ids, sid)

      shiny::insertUI(
        selector = "#vr_sort_container",
        where    = "beforeEnd",
        ui       = .vr_sort_row_ui(sid, all_cols)
      )

      local({
        local_sid <- sid
        shiny::observeEvent(
          input[[paste0("s_remove_", local_sid)]],
          {
            shiny::removeUI(selector = paste0("#sort_row_", local_sid))
            s$sort_ids <- setdiff(s$sort_ids, local_sid)
          },
          ignoreInit = TRUE,
          once       = TRUE
        )
      })
    })

    shiny::observeEvent(input$vr_clear_sorts, {
      for (sid in s$sort_ids) {
        shiny::removeUI(selector = paste0("#sort_row_", sid))
      }
      s$sort_ids <- integer(0)
    })


    # ==========================================================================
    # COLUMN VISIBILITY
    # ==========================================================================

    shiny::observeEvent(input$vr_cols_all, {
      shiny::updateCheckboxGroupInput(session, "vr_visible_cols",
                                      selected = all_cols)
    })

    shiny::observeEvent(input$vr_cols_none, {
      shiny::updateCheckboxGroupInput(session, "vr_visible_cols",
                                      selected = character(0))
    })


    # ==========================================================================
    # FIND & REPLACE
    # ==========================================================================

    shiny::observeEvent(input$vr_fnr_preview, {
      shiny::req(nzchar(input$vr_fnr_find %||% ""))

      result <- .vr_find_replace(
        data      = s$data_work,
        col_spec  = input$vr_fnr_col %||% "__all__",
        find      = input$vr_fnr_find,
        replace   = input$vr_fnr_replace %||% "",
        case_sens = isTRUE(input$vr_fnr_case),
        use_regex = isTRUE(input$vr_fnr_regex),
        exact     = isTRUE(input$vr_fnr_exact),
        preview   = TRUE
      )
      s$fnr_preview <- result$preview
    })

    output$vr_fnr_preview_tbl <- DT::renderDataTable({
      shiny::req(!is.null(s$fnr_preview), nrow(s$fnr_preview) > 0)
      DT::datatable(
        s$fnr_preview,
        rownames  = FALSE,
        class     = "compact stripe",
        options   = list(pageLength = 10, dom = "tp", scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$vr_fnr_apply, {
      shiny::req(nzchar(input$vr_fnr_find %||% ""))

      result <- .vr_find_replace(
        data      = s$data_work,
        col_spec  = input$vr_fnr_col %||% "__all__",
        find      = input$vr_fnr_find,
        replace   = input$vr_fnr_replace %||% "",
        case_sens = isTRUE(input$vr_fnr_case),
        use_regex = isTRUE(input$vr_fnr_regex),
        exact     = isTRUE(input$vr_fnr_exact),
        preview   = FALSE
      )

      if (result$n_replaced > 0) {
        s$data_work  <- result$data
        s$fnr_preview <- NULL
        if (!is.null(result$code)) {
          s$fnr_ops <- c(s$fnr_ops, result$code)
        }
        shiny::showNotification(
          paste0("\u2713 Replaced ", result$n_replaced,
                 " occurrence(s)."),
          type = "message", duration = 3
        )
      } else {
        shiny::showNotification("No matches found.", type = "warning",
                                duration = 3)
      }
    })


    # ==========================================================================
    # VARIABLE INFO TABLE
    # ==========================================================================

    output$vr_var_info_tbl <- DT::renderDataTable({
      info <- .vr_var_info(s$data_work, labels)
      DT::datatable(
        info,
        rownames  = FALSE,
        class     = "compact stripe hover",
        options   = list(
          pageLength = 25,
          scrollX    = TRUE,
          dom        = "frtip"
        )
      ) |>
        DT::formatStyle(
          "Missing %",
          background = DT::styleColorBar(c(0, 100), "#f28b82"),
          backgroundSize    = "100% 90%",
          backgroundRepeat  = "no-repeat",
          backgroundPosition = "center"
        )
    })


    # ==========================================================================
    # R CODE GENERATION
    # ==========================================================================

    if (gen_code) {

      current_code <- shiny::reactive({
        vcols <- input$vr_visible_cols %||% all_cols
        .vr_build_code(
          data_name    = data_name,
          filters      = active_filters(),
          sorts        = active_sorts(),
          visible_cols = vcols,
          all_cols     = all_cols,
          fnr_ops      = s$fnr_ops,
          edit_ops     = s$edit_ops
        )
      })

      output$vr_code_output <- shiny::renderText({
        current_code()
      })

      shiny::observeEvent(input$vr_code_copy, {
        code_txt <- shiny::isolate(current_code())
        # Encode for safe JS injection
        escaped  <- jsonlite::toJSON(code_txt, auto_unbox = TRUE)
        shinyjs::runjs(paste0("vrCopyCode(", escaped, ");"))
      })

      shiny::observeEvent(input$vr_clipboard_done, {
        shiny::showNotification("\u2713 Code copied to clipboard!",
                                type = "message", duration = 2)
      })

      shiny::observeEvent(input$vr_code_reset, {
        s$fnr_ops  <- character(0)
        s$edit_ops <- character(0)
        shiny::showNotification("Code history cleared.", type = "message",
                                duration = 2)
      })
    }


    # ==========================================================================
    # DONE / CANCEL
    # ==========================================================================

    shiny::observeEvent(input$vr_done, {
      shiny::stopApp(s$data_work)
    })

    shiny::observeEvent(input$vr_cancel, {
      shiny::stopApp(NULL)
    })

  }  # end server function
}
