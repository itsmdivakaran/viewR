# =============================================================================
# ViewR -- ui.R
# Builds the complete Shiny UI for the popup/gadget.
# =============================================================================


#' Build the ViewR Shiny UI
#' @param data          original data.frame
#' @param title         window title string
#' @param theme         shinythemes theme name
#' @param edit_mode     logical - show Edit tab
#' @param gen_code      logical - show R Code tab
#' @noRd
.vr_ui <- function(data, title, theme, edit_mode, gen_code) {

  col_names <- names(data)

  shiny::fluidPage(
    theme = shinythemes::shinytheme(theme),
    shinyjs::useShinyjs(),

    # -- Custom CSS ------------------------------------------------------------
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(.vr_css(theme))),
      shiny::tags$script(shiny::HTML(.vr_js()))
    ),

    # -- Top bar ---------------------------------------------------------------
    shiny::div(
      class = "vr-topbar",
      shiny::div(
        class = "vr-title",
        shiny::icon("table"), " ", title
      ),
      shiny::div(
        class = "vr-status",
        shiny::textOutput("vr_status", inline = TRUE)
      ),
      shiny::div(
        class = "vr-actions",
        shiny::actionButton("vr_done",   "Done",
                            class = "btn-success btn-sm",
                            icon  = shiny::icon("check")),
        shiny::actionButton("vr_cancel", "Cancel",
                            class = "btn-default btn-sm",
                            icon  = shiny::icon("times"))
      )
    ),

    # -- Body: sidebar + main panel --------------------------------------------
    shiny::fluidRow(

      # ====================================
      # SIDEBAR
      # ====================================
      shiny::column(
        width = 3,
        class = "vr-sidebar",

        # -- Filters ----------------------------------------------------------
        shiny::div(
          class = "vr-panel",
          shiny::div(
            class = "vr-panel-header",
            shiny::icon("filter"), "Filters",
            shiny::div(
              class = "pull-right",
              shiny::actionButton("vr_add_filter", NULL,
                                  icon  = shiny::icon("plus"),
                                  class = "btn-xs btn-success",
                                  title = "Add a filter condition")
            )
          ),
          shiny::div(id = "vr_filter_container"),
          shiny::conditionalPanel(
            condition = "output.vr_has_filters",
            shiny::div(
              class = "mt-2",
              style = "text-align:right",
              shiny::actionButton("vr_clear_filters", "Clear all",
                                  icon  = shiny::icon("trash"),
                                  class = "btn-xs btn-warning")
            )
          )
        ),

        # -- Sort --------------------------------------------------------------
        shiny::div(
          class = "vr-panel",
          shiny::div(
            class = "vr-panel-header",
            shiny::icon("sort"), "Sort",
            shiny::div(
              class = "pull-right",
              shiny::actionButton("vr_add_sort", NULL,
                                  icon  = shiny::icon("plus"),
                                  class = "btn-xs btn-success",
                                  title = "Add a sort level")
            )
          ),
          shiny::div(id = "vr_sort_container"),
          shiny::conditionalPanel(
            condition = "output.vr_has_sorts",
            shiny::div(
              class = "mt-2",
              style = "text-align:right",
              shiny::actionButton("vr_clear_sorts", "Clear all",
                                  icon  = shiny::icon("trash"),
                                  class = "btn-xs btn-warning")
            )
          )
        ),

        # -- Column visibility -------------------------------------------------
        shiny::div(
          class = "vr-panel",
          shiny::div(
            class = "vr-panel-header",
            shiny::icon("columns"), "Columns",
            shiny::div(
              class = "pull-right",
              shiny::actionButton("vr_cols_all",  "All",
                                  class = "btn-xs btn-link mb-0"),
              shiny::actionButton("vr_cols_none", "None",
                                  class = "btn-xs btn-link mb-0")
            )
          ),
          shiny::div(
            class = "vr-col-list",
            shiny::checkboxGroupInput(
              inputId  = "vr_visible_cols",
              label    = NULL,
              choices  = setNames(col_names, col_names),
              selected = col_names
            )
          )
        ),

        # -- Display options ---------------------------------------------------
        shiny::div(
          class = "vr-panel",
          shiny::div(class = "vr-panel-header",
                     shiny::icon("sliders-h"), "Display"),
          shiny::sliderInput("vr_page_length", "Rows per page:",
                             min = 5, max = 500, value = 25, step = 5),
          shiny::checkboxInput("vr_show_labels",
                               "Show variable labels", value = TRUE),
          shiny::checkboxInput("vr_show_rownames",
                               "Show row index", value = TRUE)
        )
      ),   # end sidebar column

      # ====================================
      # MAIN PANEL
      # ====================================
      shiny::column(
        width = 9,

        shiny::tabsetPanel(
          id = "vr_tabs",

          # -- Data View ------------------------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("table"), " Data"),
            value = "tab_view",
            shiny::br(),
            DT::dataTableOutput("vr_table")
          ),

          # -- Edit (Excel-like) ----------------------------------------------
          if (edit_mode) shiny::tabPanel(
            title = shiny::tagList(shiny::icon("edit"), " Edit"),
            value = "tab_edit",
            shiny::br(),
            shiny::div(
              class = "vr-edit-toolbar",
              shiny::actionButton("vr_add_row", "Add Row",
                                  icon  = shiny::icon("plus-circle"),
                                  class = "btn-sm btn-success"),
              shiny::actionButton("vr_del_row", "Delete Selected",
                                  icon  = shiny::icon("minus-circle"),
                                  class = "btn-sm btn-danger"),
              shiny::actionButton("vr_edit_undo", "Undo",
                                  icon  = shiny::icon("undo"),
                                  class = "btn-sm btn-default"),
              shiny::actionButton("vr_edit_redo", "Redo",
                                  icon  = shiny::icon("redo"),
                                  class = "btn-sm btn-default"),
              shiny::tags$span(
                class = "text-muted-sm",
                style = "align-self:center; padding-left:6px;",
                "Click a cell to edit. Changes apply when you click \u2018Done\u2019."
              )
            ),
            rhandsontable::rHandsontableOutput("vr_hot",
                                               height = "500px")
          ),

          # -- Find & Replace -------------------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("search"), " Find & Replace"),
            value = "tab_fnr",
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                5,
                shiny::wellPanel(
                  shiny::h5(shiny::icon("exchange-alt"), " Find & Replace"),
                  shiny::selectInput(
                    "vr_fnr_col", "Search in:",
                    choices = c("All columns" = "__all__",
                                setNames(col_names, col_names))
                  ),
                  shiny::textInput("vr_fnr_find",    "Find:"),
                  shiny::textInput("vr_fnr_replace", "Replace with:"),
                  shiny::fluidRow(
                    shiny::column(
                      6,
                      shiny::checkboxInput("vr_fnr_case",  "Case sensitive", FALSE),
                      shiny::checkboxInput("vr_fnr_regex", "Regex",          FALSE)
                    ),
                    shiny::column(
                      6,
                      shiny::checkboxInput("vr_fnr_exact", "Exact cell match", FALSE)
                    )
                  ),
                  shiny::div(
                    style = "display:flex; gap:6px; margin-top:6px;",
                    shiny::actionButton("vr_fnr_preview", "Preview",
                                        icon  = shiny::icon("eye"),
                                        class = "btn-default btn-sm"),
                    shiny::actionButton("vr_fnr_apply", "Apply",
                                        icon  = shiny::icon("check"),
                                        class = "btn-primary btn-sm")
                  )
                )
              ),
              shiny::column(
                7,
                shiny::conditionalPanel(
                  condition = "output.vr_fnr_has_preview",
                  shiny::div(
                    shiny::tags$b("Preview of changes"),
                    shiny::tags$small(
                      class = "text-muted",
                      " (showing matched cells only):"
                    ),
                    shiny::br(), shiny::br(),
                    DT::dataTableOutput("vr_fnr_preview_tbl")
                  )
                ),
                shiny::conditionalPanel(
                  condition = "!output.vr_fnr_has_preview",
                  shiny::div(
                    class = "text-muted-sm",
                    style = "padding:20px 0; color:#aaa; text-align:center;",
                    shiny::icon("search"),
                    " Enter a search term and click Preview to see matches."
                  )
                )
              )
            )
          ),

          # -- Variable Info --------------------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("info-circle"), " Variable Info"),
            value = "tab_info",
            shiny::br(),
            DT::dataTableOutput("vr_var_info_tbl")
          ),

          # -- R Code --------------------------------------------------------
          if (gen_code) shiny::tabPanel(
            title = shiny::tagList(shiny::icon("code"), " R Code"),
            value = "tab_code",
            shiny::br(),
            shiny::div(
              class = "vr-code-toolbar",
              shiny::actionButton("vr_code_copy",  "Copy to clipboard",
                                  icon  = shiny::icon("copy"),
                                  class = "btn-primary btn-sm"),
              shiny::actionButton("vr_code_reset", "Reset",
                                  icon  = shiny::icon("undo"),
                                  class = "btn-warning btn-sm"),
              shiny::tags$span(
                class = "text-muted-sm",
                style = "align-self:center; padding-left:4px;",
                "Code reflects current filters, sorts, column selection, and edits."
              )
            ),
            shiny::verbatimTextOutput("vr_code_output")
          )

        )  # end tabsetPanel
      )   # end main column
    )    # end fluidRow
  )     # end fluidPage
}


# -- Inline JavaScript ---------------------------------------------------------
.vr_js <- function() {
  "
// Copy-to-clipboard helper called from R via shinyjs::runjs()
function vrCopyCode(text) {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text).then(function() {
      Shiny.setInputValue('vr_clipboard_done', Math.random());
    });
  } else {
    // Fallback for older browsers
    var ta = document.createElement('textarea');
    ta.value = text;
    document.body.appendChild(ta);
    ta.select();
    document.execCommand('copy');
    document.body.removeChild(ta);
    Shiny.setInputValue('vr_clipboard_done', Math.random());
  }
}
  "
}
