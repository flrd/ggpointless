# Static checks against the rendered Rd files.
# Catches doc-merge bugs that would otherwise show up only on visual
# inspection of the help pages or pkgdown site.

rd_root <- testthat::test_path("..", "..", "man")

# Helper: read an Rd, return contents of the body of a top-level
# brace-delimited section (e.g. \value{...} or \examples{...}).
.rd_section_body <- function(src, section) {
  s <- grep(paste0("^\\\\", section, "\\{$"), src)
  if (!length(s)) return(NULL)
  start <- s[1] + 1L
  e <- start
  while (e <= length(src) && src[e] != "}") e <- e + 1L
  src[start:(e - 1L)]
}

test_that("each Rd \\value{} block has exactly one paragraph", {
  # Rule (per CLAUDE.md): for every @rdname group, only the primary R file
  # should declare @return. Sibling files using @rdname must NOT also have
  # @return -- otherwise the merged Rd ends up with duplicated \value{}
  # paragraphs (visible to users on the help page).
  if (!dir.exists(rd_root)) skip("man/ not available")

  audit <- function(rd_path) {
    body <- .rd_section_body(readLines(rd_path, warn = FALSE), "value")
    if (is.null(body)) return(NULL)
    is_blank <- !nzchar(trimws(body))
    n_para <- sum(rle(!is_blank)$values)
    if (n_para > 1L) {
      sprintf("%s: %d paragraphs", basename(rd_path), n_para)
    } else NULL
  }
  rds <- list.files(rd_root, pattern = "\\.Rd$", full.names = TRUE)
  flagged <- unlist(lapply(rds, audit))
  expect_null(
    flagged,
    info = paste(
      "Duplicated @return tags merged into one Rd. Drop @return from",
      "@rdname-sibling files; keep it only in the primary."
    )
  )
})

test_that("each Rd has no duplicate \\section{}{} titles", {
  # Rule: a given @section title must appear only once per @rdname group.
  # `@inheritSection X Y` from a sibling file plus a manual `@section Y:`
  # in the primary would otherwise create two `\section{Y}{}` blocks in
  # the merged Rd. Roxygen sometimes deduplicates, sometimes doesn't --
  # and pkgdown shows both as separate headings. Catch it here.
  if (!dir.exists(rd_root)) skip("man/ not available")

  audit <- function(rd_path) {
    src <- readLines(rd_path, warn = FALSE)
    titles <- regmatches(
      src,
      regexpr("^\\\\section\\{[^}]+\\}", src)
    )
    titles <- titles[nzchar(titles)]
    if (!length(titles)) return(NULL)
    dup <- titles[duplicated(titles)]
    if (length(dup)) {
      sprintf("%s: %s", basename(rd_path), paste(unique(dup), collapse = ", "))
    } else NULL
  }
  rds <- list.files(rd_root, pattern = "\\.Rd$", full.names = TRUE)
  flagged <- unlist(lapply(rds, audit))
  expect_null(
    flagged,
    info = paste(
      "Duplicate \\section{} titles in one Rd. Drop the @inheritSection",
      "from the sibling file that already inherits via @rdname, OR remove",
      "the manual @section: from the primary if the sibling is the source."
    )
  )
})

test_that("each Rd has no duplicate paragraphs within a section/references/seealso", {
  # Rule: roxygen2 merges multiple `@section X:` / `@inheritSection ... X`
  # entries with the same title into ONE `\section{X}{...}` block — the
  # heading appears once (so the duplicate-titles test passes), but the
  # bodies are concatenated. The same merge happens for `@references` and
  # `@aesthetics` blocks contributed by sibling files. Catch the resulting
  # duplicated paragraphs here.
  if (!dir.exists(rd_root)) skip("man/ not available")

  # Block openers we care about. Single-arg form (`\references{`,
  # `\seealso{`) plus two-arg form (`\section{TITLE}{`). Lines must end
  # with `{` so single-line macros (`\concept{x}`) don't get treated as
  # block starters.
  block_re <- "^\\\\([a-zA-Z]+)(\\{[^{}]*\\})?\\{$"
  keep_names <- c("section", "references", "seealso")

  # Normalise a paragraph so paragraphs that differ only in an inline
  # identifier (e.g. the geom name in `\code{geom_X_fade()}`) count as
  # duplicates. Replace the payload of inline markup with `X`.
  .normalise_para <- function(lines) {
    s <- paste(lines, collapse = " ")
    repeat {
      s2 <- gsub("\\\\code\\{[^{}]*\\}", "\\\\code{X}", s)
      s2 <- gsub("\\\\verb\\{[^{}]*\\}", "\\\\verb{X}", s2)
      s2 <- gsub("\\\\link\\[[^]]*\\]\\{[^{}]*\\}", "\\\\link{X}", s2)
      s2 <- gsub("\\\\emph\\{[^{}]*\\}", "\\\\emph{X}", s2)
      s2 <- gsub("\\\\strong\\{[^{}]*\\}", "\\\\strong{X}", s2)
      if (identical(s2, s)) break
      s <- s2
    }
    trimws(gsub("\\s+", " ", s))
  }

  .paragraphs <- function(lines) {
    paras <- list()
    cur <- character()
    flush <- function() {
      if (length(cur)) {
        paras[[length(paras) + 1L]] <<- cur
        cur <<- character()
      }
    }
    for (ln in lines) {
      if (nzchar(trimws(ln))) cur <- c(cur, ln) else flush()
    }
    flush()
    paras
  }

  # Walk from a block-opener line, counting `{` and `}` per line until the
  # outer block's brace depth returns to 0. Returns the body line range,
  # excluding the opener and the closing `}` line.
  .block_body <- function(src, start_idx) {
    depth <- 0L
    for (i in start_idx:length(src)) {
      ln <- src[i]
      depth <- depth +
        nchar(gsub("[^{]", "", ln)) -
        nchar(gsub("[^}]", "", ln))
      if (depth <= 0L && i > start_idx) {
        return(if (i - 1L >= start_idx + 1L) src[(start_idx + 1L):(i - 1L)] else character())
      }
    }
    src[(start_idx + 1L):length(src)]
  }

  audit <- function(rd_path) {
    src <- readLines(rd_path, warn = FALSE)
    starts <- grep(block_re, src)
    if (!length(starts)) return(NULL)
    names_at <- vapply(
      regmatches(src[starts], regexec(block_re, src[starts])),
      function(m) m[2L], character(1)
    )
    keep <- names_at %in% keep_names
    starts <- starts[keep]
    names_at <- names_at[keep]
    if (!length(starts)) return(NULL)

    flagged <- character()
    for (k in seq_along(starts)) {
      body <- .block_body(src, starts[k])
      paras <- .paragraphs(body)
      if (length(paras) < 2L) next
      norm <- vapply(paras, .normalise_para, character(1))
      norm <- norm[nzchar(norm)]
      dups <- unique(norm[duplicated(norm)])
      if (length(dups)) {
        flagged <- c(
          flagged,
          sprintf(
            "%s [\\%s]: %d duplicate paragraph(s)",
            basename(rd_path), names_at[k], length(dups)
          )
        )
      }
    }
    if (length(flagged)) flagged else NULL
  }
  rds <- list.files(rd_root, pattern = "\\.Rd$", full.names = TRUE)
  flagged <- unlist(lapply(rds, audit))
  expect_null(
    flagged,
    info = paste(
      "Identical paragraphs repeated inside one \\section{}/\\references{}/",
      "\\seealso{} body. Likely cause: `@section X:` in the primary file",
      "AND `@inheritSection primary X` in a sibling under the same @rdname,",
      "or `@aesthetics GeomFoo` / `@references` repeated across siblings.",
      "Keep these tags only in the primary @rdname file."
    )
  )
})

test_that("each Rd \\examples{} block starts with library(ggplot2)", {
  # Convention: every help page's first executable line should be
  # `library(ggplot2)`. Users copy-pasting from one example often skip the
  # `library(ggpointless)` call already loaded in their session, but they
  # may not have ggplot2 attached -- the example then fails at the first
  # `ggplot()` call with "could not find function". For @rdname-merged
  # pages this means the FIRST contributing R file (per Collate order)
  # should put `library(ggplot2)` at the top of its @examples; sibling
  # files do NOT need their own to avoid duplication.
  if (!dir.exists(rd_root)) skip("man/ not available")

  audit <- function(rd_path) {
    body <- .rd_section_body(readLines(rd_path, warn = FALSE), "examples")
    if (is.null(body)) return(NULL)
    body <- body[nzchar(trimws(body))]
    if (!length(body)) return(NULL)
    if (body[1] != "library(ggplot2)") {
      sprintf("%s: first line is '%s'", basename(rd_path), body[1])
    } else NULL
  }
  rds <- list.files(rd_root, pattern = "\\.Rd$", full.names = TRUE)
  flagged <- unlist(lapply(rds, audit))
  expect_null(
    flagged,
    info = paste(
      "An Rd \\examples{} block does not start with library(ggplot2).",
      "Add it to the @examples block in the primary R file (the one whose",
      "Collate position is earliest among the @rdname siblings)."
    )
  )
})
