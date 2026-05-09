box::use(
    rlang[parse_expr, eval_tidy, new_environment]
)

#' Parse a user-supplied PDF string into a safe callable function
#'
#' Only math and base distribution functions are whitelisted.
#' Anything else (system calls, file I/O, assignment) is blocked.
#'
#' @param expr_string Character. The user's input, e.g. "1 - abs(x)".
#'
#' @return A function f(x) if valid, or NULL with a warning if not.
#' @export
parse_pdf = function(expr_string) {
    dangerous = c(
        "system\\s*\\(",
        "file\\s*\\(",
        "readLines", "writeLines",
        "source\\s*\\(",
        "eval\\s*\\(",
        "parse\\s*\\(",
        "Sys\\.",
        "proc\\.time",
        "<<-", "<-", "->",
        "library\\s*\\(",
        "require\\s*\\(",
        "install\\.packages"
    )

    blocked = vapply(
        dangerous,
        \(p) grepl(p, expr_string, fixed = FALSE),
        logical(1)
    )

    if (any(blocked)) {
        warning("Blocked expression: contains disallowed operations.")
        return(NULL)
    }

    safe_env = new_environment(
        data = list(
            x = NULL,
            abs = base::abs,
            exp = base::exp,
            log = base::log,
            sqrt = base::sqrt,
            sin = base::sin,
            cos = base::cos,
            tan = base::tan,
            pi = base::pi,
            dnorm = stats::dnorm,
            dgamma = stats::dgamma,
            dunif = stats::dunif,
            dbeta = stats::dbeta,
            dexp = stats::dexp,
            dpois = stats::dpois
        ),
        parent = baseenv()
    )

    parsed = tryCatch(
        rlang::parse_expr(expr_string),
        error = function(e) {
            warning(paste("Could not parse expression:", e$message))
            return(NULL)
        }
    )

    if (is.null(parsed)) return(NULL)

    function(x) {
        safe_env$x = x
        tryCatch(
            rlang::eval_tidy(parsed, env = safe_env),
            error = function(e) {
                warning(paste("PDF evaluation error:", e$message))
                rep(NA_real_, length(x))
            }
        )
    }
}
