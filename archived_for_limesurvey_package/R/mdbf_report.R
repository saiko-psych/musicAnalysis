#' Write (and optionally render) a Quarto report for MDBF scales & reliability
#'
#' @param path File path for the .qmd to write. Default "reports/mdbf_report.qmd".
#' @param root Optional folder for CSVs; used if `dat_code` is NULL.
#' @param dat_code Optional R code that creates an object `dat`. If NULL, the template calls
#'   `limesurvey_pre(root = ..., deduplicate = "latest")`.
#' @param render One of c("none","html","pdf"). If not "none", tries to render via `quarto`.
#' @return (Invisibly) the written file path.
#' @export
mdbf_write_report <- function(path = "reports/mdbf_report.qmd",
                              root = NULL,
                              dat_code = NULL,
                              render = c("none","html","pdf")) {
  render <- match.arg(render)

  # Datenblock
  if (is.null(dat_code)) {
    if (is.null(root)) rlang::abort("Provide either `dat_code` or `root`.")
    dat_lines <- c(
      "```{r}",
      "library(musicAnalysis)",
      sprintf('dat <- limesurvey_pre(root = "%s", deduplicate = "latest")', root),
      "```"
    )
  } else {
    dat_lines <- c(
      "```{r}",
      "library(musicAnalysis)",
      dat_code,
      "```"
    )
  }

  # QMD-Zeilen (kein großer String mit ``` drin)
  qmd_lines <- c(
    "---",
    'title: "MDBF – Skalen & Reliabilität"',
    "format:",
    "  html:",
    "    toc: true",
    "    code-fold: show",
    "execute:",
    "  echo: true",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "## Daten laden",
    "",
    dat_lines,
    "",
    "## Skalen berechnen",
    "",
    "```{r}",
    "mdbf <- mdbf_scales(dat)",
    "mdbf$alpha_summary",
    "```",
    "",
    "## Reliabilitätsübersicht",
    "",
    "```{r}",
    "mdbf$alpha_summary",
    "```",
    "",
    "## Subskalen – Cronbach's Alpha (Totals)",
    "",
    "```{r}",
    "mdbf$alpha$GS$total",
    "mdbf$alpha$WM$total",
    "mdbf$alpha$RU$total",
    "mdbf$alpha$Total$total",
    "```",
    "",
    "## Item-Statistik (GS)",
    "",
    "```{r}",
    "mdbf$alpha$GS$item.stats",
    "```",
    "",
    "## Item-Statistik (WM)",
    "",
    "```{r}",
    "mdbf$alpha$WM$item.stats",
    "```",
    "",
    "## Item-Statistik (RU)",
    "",
    "```{r}",
    "mdbf$alpha$RU$item.stats",
    "```",
    "",
    "## Item-Statistik (Total)",
    "",
    "```{r}",
    "mdbf$alpha$Total$item.stats",
    "```",
    "",
    "## Hinweise",
    "",
    "- Reverse-Codierung: x_rev = (max + min) - x (Endpunkte aus allen 24 Items ermittelt).",
    "- GS: MDBFL1, MDBFL8, MDBFL14, MDBFL21, MDBFL11, MDBFL4, MDBFL16, MDBFL18",
    "- WM: MDBFL2, MDBFL10, MDBFL20, MDBFL17, MDBFL5, MDBFL7, MDBFL13, MDBFL23",
    "- RU: MDBFL6, MDBFL12, MDBFL15, MDBFL24, MDBFL3, MDBFL9, MDBFL19, MDBFL22"
  )

  # schreiben (UTF-8)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(qmd_lines, con = con, useBytes = TRUE)
  cli::cli_inform("Quarto report written to {.path {path}}.")

  # optional render
  if (render != "none") {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      cli::cli_warn(sprintf("`render = '%s'` requested, but package 'quarto' not available. Skipping render.", render))
      return(invisible(path))
    }
    fmt <- if (render == "pdf") "pdf" else "html"
    quarto::quarto_render(path, output_format = fmt)
    cli::cli_inform("Rendered report to %s.", fmt)
  }

  invisible(path)
}
