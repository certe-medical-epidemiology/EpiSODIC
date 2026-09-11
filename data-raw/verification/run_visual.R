# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

# The browser half of the verification pass. See README.md in this folder.
#
# Everything here is an assertion a string comparison against the
# stylesheet cannot make: whether a control is actually on screen,
# whether the element at its centre is the control or something covering
# it, how tall the rendered box really is, and whether a piece of state
# survives a navigation. Those are the defects this navigation was
# rebuilt to remove, and a rendering engine is the only thing that can
# say whether they are gone.
#
# Exits 0 if every assertion held and 1 if any did not.

options(warn = 1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

episodic_script_dir <- function() {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", arg))))
  }
  getwd()
}
root <- normalizePath(file.path(episodic_script_dir(), "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(getwd())
}
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run this from the package root, or from data-raw/verification/.")
}
setwd(root)

for (pkg in c("chromote", "callr", "devtools")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is needed. See README.md in this folder.")
  }
}

out_dir <- file.path(root, "data-raw", "verification", "output")
shot_dir <- file.path(out_dir, "screenshots")
dir.create(shot_dir, showWarnings = FALSE, recursive = TRUE)

# Three by default. `Rscript run_visual.R all` does all eight, which is
# the right thing before a release and too slow to be the default.
LANGS <- if (identical(commandArgs(trailingOnly = TRUE)[1], "all")) {
  c("en", "nl", "de", "fr", "es", "ar", "hi", "zh")
} else {
  # English for the reading, Dutch for the longest labels, Arabic
  # because it is the one that mirrors.
  c("en", "nl", "ar")
}

VIEWPORTS <- list(
  list(name = "phone", width = 360, height = 780, mobile = TRUE),
  list(name = "phone-large", width = 414, height = 896, mobile = TRUE),
  list(name = "tablet", width = 820, height = 1180, mobile = TRUE),
  list(name = "laptop", width = 1280, height = 800, mobile = FALSE),
  list(name = "desktop", width = 1920, height = 1080, mobile = FALSE)
)

PORT <- 7391L

# --------------------------------------------------------------------- #
# Results                                                               #
# --------------------------------------------------------------------- #

results <- list()
record <- function(context, name, ok, detail = "") {
  # An absent shell attribute (data-pane, data-access, ...) reaches here
  # as NULL by way of shell_attr()'s getAttribute() returning JS null -
  # exactly the detail worth recording on a failing check, not a reason
  # to crash the run before it is shown.
  detail <- detail %||% "NULL"
  results[[length(results) + 1L]] <<- list(
    context = context,
    name = name,
    ok = isTRUE(ok),
    detail = detail
  )
  if (!isTRUE(ok)) {
    message(sprintf(
      "  FAIL  %s :: %s%s", context, name,
      if (nzchar(detail)) paste0(" - ", detail) else ""
    ))
  }
}

# --------------------------------------------------------------------- #
# The instance under test                                               #
# --------------------------------------------------------------------- #

message("Building a demo database (this is the slow part) ...")
devtools::load_all(root, quiet = TRUE)

db_path <- file.path(tempdir(), "episodic-verification.sqlite")
unlink(db_path)
invisible(episodic_demo(db_path = db_path, launch = FALSE, overwrite = TRUE))
demo_files <- EpiSODIC:::episodic_demo_files(db_path)

# The app ships closed to anonymous visitors, and a locked screen shows
# none of the navigation this script exists to measure. So the instance
# under test is opened deliberately, in a config of its own: these are
# assertions about layout, not about the access policy. The locked screen
# gets a pass of its own at the end, with the shipped default.
open_config <- file.path(tempdir(), "episodic-verification-open.yaml")
writeLines(
  c(
    readLines(demo_files$config, warn = FALSE),
    "access:",
    "  require_login: false"
  ),
  open_config
)

app_process <- NULL
start_app <- function(lang, config) {
  if (!is.null(app_process) && app_process$is_alive()) {
    app_process$kill()
  }
  p <- callr::r_bg(
    function(root, db_path, config, pc_map, lang, port) {
      Sys.setenv(
        EPISODIC_DB = db_path,
        EPISODIC_CONFIG = config,
        EPISODIC_PC_PROVINCE_MAP = pc_map,
        EPISODIC_LANGUAGE = lang
      )
      pkgload::load_all(root, quiet = TRUE)
      # load_all() also sources tests/testthat/helper-*.R, for the
      # convenience of using the suite's own fixtures at an interactive
      # console. helper-config.R's own top-level code does exactly that -
      # it points EPISODIC_CONFIG at a test config of its own so the rest
      # of the suite runs unlocked - which silently replaces the config
      # this harness was asked to run under, unlocked-vs-locked included.
      # Reasserted here, after load_all() and before the app starts, so
      # this session runs under the config named above rather than the
      # suite's.
      Sys.setenv(
        EPISODIC_DB = db_path,
        EPISODIC_CONFIG = config,
        EPISODIC_PC_PROVINCE_MAP = pc_map,
        EPISODIC_LANGUAGE = lang
      )
      # The package's own entry point, not a shinyApp() assembled here:
      # it is what registers the www/ resource path, and a harness that
      # serves the stylesheet and the navigation script differently from
      # the way a deployment does is a harness measuring something else.
      episodic_run_app(
        db_path = db_path,
        lang = lang,
        port = port,
        host = "127.0.0.1",
        launch.browser = FALSE
      )
    },
    args = list(
      root = root,
      db_path = db_path,
      config = config,
      pc_map = demo_files$pc_province_map,
      lang = lang,
      port = PORT
    ),
    supervise = TRUE
  )
  # Wait for the port to answer rather than sleeping a fixed time: a
  # cold start on a loaded machine is a lot slower than on an idle one,
  # and a fixed sleep is either wasteful or flaky.
  deadline <- Sys.time() + 90
  repeat {
    if (!p$is_alive()) {
      stop("The app process exited: ", paste(p$read_all_error_lines(), collapse = "\n"))
    }
    ok <- tryCatch(
      {
        con <- url(sprintf("http://127.0.0.1:%d/", PORT), open = "rb")
        on.exit(close(con), add = TRUE)
        length(readBin(con, "raw", n = 1L)) == 1L
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (ok) break
    if (Sys.time() > deadline) {
      stop("The app did not start within 90 seconds.")
    }
    Sys.sleep(0.4)
  }
  p
}

# No on.exit here: at the top level of a script each statement is its own
# frame, so an exit handler registered here would run immediately and
# guarantee nothing. `callr::r_bg(supervise = TRUE)` is what actually
# takes the app process down with this one, however this ends.

# --------------------------------------------------------------------- #
# Driving the browser                                                   #
# --------------------------------------------------------------------- #

new_session <- function() {
  b <- chromote::ChromoteSession$new()
  # A JavaScript error is exactly what "it sometimes doesn't respond"
  # looks like from the outside, so it is collected rather than left in
  # a console nobody opens.
  b$Page$addScriptToEvaluateOnNewDocument(source = paste0(
    "window.__episodicErrors = [];",
    "window.addEventListener('error', function (e) {",
    "  window.__episodicErrors.push(String(e.message));",
    "});",
    "window.addEventListener('unhandledrejection', function (e) {",
    "  window.__episodicErrors.push('unhandled rejection: ' + String(e.reason));",
    "});"
  ))
  b
}

js <- function(b, expr) {
  res <- b$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE)
  if (!is.null(res$exceptionDetails)) {
    stop("JavaScript error: ", res$exceptionDetails$text, " in: ", expr)
  }
  res$result$value
}

wait_until <- function(b, expr, timeout = 30, what = expr) {
  deadline <- Sys.time() + timeout
  repeat {
    if (isTRUE(js(b, expr))) {
      # A screen shown by a pure attribute flip settles by this
      # predicate before shiny (>= 1.14.0)'s own visibility-driven
      # bind/resume of the newly-shown output has finished its own
      # microtask tail - a click landing in that gap is a click the
      # delegated listener never sees. One confirming read, a beat
      # later, is what a real end-to-end harness calls settled rather
      # than "true on one poll".
      Sys.sleep(0.1)
      if (isTRUE(js(b, expr))) {
        return(invisible(TRUE))
      }
    }
    if (Sys.time() > deadline) {
      stop("Timed out waiting for: ", what)
    }
    Sys.sleep(0.15)
  }
}

# Shiny is finished with a flush when nothing *visible* is still
# recalculating and the page has stopped being busy. Visible, because
# shiny (>= 1.14.0, see DESCRIPTION) suspends a bound output for as long
# as its element is hidden - every screen but the one on show, and every
# phone-only control at a non-phone width - and never computes it until
# it is. Waiting on the full `.recalculating` count instead would wait
# for screens nothing has navigated to yet, forever.
settled <- paste0(
  "(function () {",
  "  var stuck = Array.prototype.filter.call(",
  "    document.querySelectorAll('.recalculating'),",
  "    function (el) {",
  "      return el.offsetWidth > 0 || el.offsetHeight > 0 || el.getClientRects().length > 0;",
  "    }",
  "  );",
  "  return stuck.length === 0 &&",
  "         !document.body.classList.contains('shiny-busy');",
  "})()"
)

open_app <- function(b) {
  # This runs a navigation per viewport per language - forty-odd over a
  # full pass - on whatever else the machine is doing (see README.md:
  # weekly, unattended, never the critical path of anything). A `Page`
  # load event that does not fire within chromote's default window is
  # more often that than a broken navigation, so it gets one retry with
  # room to breathe before this counts as a failure.
  loaded <- FALSE
  for (attempt in 1:2) {
    ok <- tryCatch(
      {
        b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT))
        b$Page$loadEventFired(wait_ = TRUE, timeout_ = 30)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      loaded <- TRUE
      break
    }
  }
  if (!loaded) {
    stop("The app page did not load after two attempts.")
  }
  wait_until(
    b,
    "!!document.querySelector('.episodic-shell')",
    what = "the app shell"
  )
  wait_until(b, settled, timeout = 60, what = "the first flush to settle")
  invisible(TRUE)
}

set_viewport <- function(b, vp) {
  b$Emulation$setDeviceMetricsOverride(
    width = vp$width,
    height = vp$height,
    deviceScaleFactor = 1,
    mobile = vp$mobile
  )
  Sys.sleep(0.35)
}

# A real hit test, not just a dispatched event: elementFromPoint answers
# with whatever is actually painted at the control's centre, so a button
# covered by a sticky bar or an invisible overlay fails here rather than
# silently swallowing presses in production.
hit_test <- function(b, selector) {
  expr <- sprintf(
    "(function () {
       var el = document.querySelector(%s);
       if (!el) return 'missing';
       var r = el.getBoundingClientRect();
       if (r.width === 0 || r.height === 0) return 'zero-size';
       var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
       if (cx < 0 || cy < 0 || cx > window.innerWidth || cy > window.innerHeight) {
         return 'off-screen';
       }
       var at = document.elementFromPoint(cx, cy);
       if (!at) return 'nothing-at-centre';
       return el.contains(at) || at.contains(el) ? 'ok' : 'covered-by:' + at.className;
     })()",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  )
  # A screen shown by a pure attribute flip (no Shiny round trip, so
  # `settled` sees nothing to wait for) still needs the browser's own
  # layout and, for the off-canvas rail, a 0.2s CSS transform transition
  # - both land a beat after the flip and both can leave a hit test
  # reading 'missing', 'zero-size' or 'covered-by' the rail still sliding
  # over it. Retried the same way for up to a second; a verdict that
  # still isn't 'ok' once that budget is spent is the real defect this
  # test exists to catch, not another frame away from being one.
  deadline <- Sys.time() + 1
  repeat {
    result <- js(b, expr)
    if (identical(result, "ok") || Sys.time() > deadline) {
      return(result)
    }
    Sys.sleep(0.03)
  }
}

click <- function(b, selector) {
  hit <- hit_test(b, selector)
  if (!identical(hit, "ok")) {
    return(hit)
  }
  js(b, sprintf(
    "(function(){document.querySelector(%s).click(); return true;})()",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ))
  "ok"
}

shell_attr <- function(b, attr) {
  js(b, sprintf(
    "document.querySelector('.episodic-shell').getAttribute('%s')",
    attr
  ))
}

# --------------------------------------------------------------------- #
# The assertions                                                        #
# --------------------------------------------------------------------- #

check_viewport <- function(b, ctx, vp, lang) {
  # 1. Every nav link is on screen. Not "in the DOM" - painted, inside
  #    the viewport, and with a box. This is what "visible always" has to
  #    mean to be worth asserting.
  links <- js(b, "
    Array.prototype.map.call(
      document.querySelectorAll('.episodic-nav-link'),
      function (a) {
        var r = a.getBoundingClientRect();
        return {
          view: a.getAttribute('data-view'),
          width: r.width,
          height: r.height,
          inside: r.left >= -1 && r.right <= window.innerWidth + 1 &&
                  r.top >= -1 && r.bottom <= window.innerHeight + 1
        };
      }
    )")
  record(
    ctx, "four nav links", length(links) == 4,
    sprintf("found %d", length(links))
  )
  for (l in links) {
    record(
      ctx,
      paste("nav link on screen:", l$view),
      l$width > 0 && l$height > 0 && isTRUE(l$inside),
      sprintf("%.0fx%.0f, inside=%s", l$width, l$height, l$inside)
    )
  }

  # 2. The page never scrolls sideways. One element with a min-width
  #    wider than the screen is enough to do it, and it is invisible on
  #    the machine it was written on.
  overflow <- js(b, "
    ({ scroll: document.documentElement.scrollWidth, inner: window.innerWidth })")
  record(
    ctx,
    "no horizontal overflow",
    overflow$scroll <= overflow$inner + 1,
    sprintf("scrollWidth %d vs innerWidth %d", overflow$scroll, overflow$inner)
  )

  # 3. Exactly one screen is visible.
  visible <- js(b, "
    Array.prototype.filter.call(
      document.querySelectorAll('.episodic-screen'),
      function (s) { return s.offsetParent !== null; }
    ).map(function (s) { return s.getAttribute('data-screen'); })")
  record(
    ctx,
    "exactly one screen visible",
    length(visible) == 1 && identical(visible[[1]], shell_attr(b, "data-view")),
    paste(unlist(visible), collapse = ", ")
  )

  # 4. Touch targets, measured from the rendered box.
  if (vp$width < 1200) {
    short <- js(b, "
      Array.prototype.map.call(
        document.querySelectorAll(
          '.episodic-nav-link, .episodic-rail-item-open, .episodic-pane-tab, .episodic-rail-toggle'
        ),
        function (el) {
          var r = el.getBoundingClientRect();
          return { cls: el.className, h: r.height, w: r.width };
        }
      ).filter(function (x) { return x.h > 0 && x.h < 44; })")
    record(
      ctx,
      "every navigation control clears 44px",
      length(short) == 0,
      if (length(short) == 0) {
        ""
      } else {
        paste(
          vapply(short, function(x) sprintf("%s=%.0fpx", x$cls, x$h), character(1)),
          collapse = ", "
        )
      }
    )
  }

  # 5. Nothing is covering anything. The rail toggle and the segmented
  #    control are fixed-position elements laid over the screen, which
  #    is exactly the arrangement that swallows presses.
  sel <- ".episodic-nav-link[data-view='clusters']"
  hit <- hit_test(b, sel)
  record(ctx, paste("hit test:", sel), identical(hit, "ok"), hit)
  if (vp$width < 768) {
    for (pane in c("rail", "dossier", "assessment")) {
      sel <- sprintf(".episodic-pane-tab[data-episodic-pane='%s']", pane)
      hit <- hit_test(b, sel)
      record(ctx, paste("hit test: pane tab", pane), identical(hit, "ok"), hit)
    }
  }
  if (vp$width >= 768 && vp$width < 1200) {
    hit <- hit_test(b, ".episodic-rail-toggle")
    record(ctx, "hit test: rail toggle", identical(hit, "ok"), hit)
  }

  # 6. Arabic mirrors. The bar starts at the right edge, not the left.
  if (identical(lang, "ar")) {
    record(
      ctx, "document direction is rtl",
      identical(js(b, "document.documentElement.dir"), "rtl")
    )
    first_right <- js(b, "
      document.querySelector('.episodic-nav-link')
        .getBoundingClientRect().right")
    record(
      ctx,
      "navigation starts at the right edge",
      first_right > js(b, "window.innerWidth") / 2,
      sprintf("first link's right edge at %.0f", first_right)
    )
  }
}

check_navigation <- function(b, ctx) {
  # Clicking a nav link changes the screen with nothing waiting on the
  # server: the attribute is read back immediately, before any flush.
  for (view in c("pathogen", "archive", "instance", "clusters")) {
    sel <- sprintf(".episodic-nav-link[data-view='%s']", view)
    hit <- click(b, sel)
    if (!identical(hit, "ok")) {
      record(ctx, paste("navigate to", view), FALSE, hit)
      next
    }
    record(
      ctx,
      paste("navigate to", view, "with no round trip"),
      identical(shell_attr(b, "data-view"), view) &&
        identical(shell_attr(b, "data-nav"), view),
      sprintf(
        "view=%s nav=%s",
        shell_attr(b, "data-view"),
        shell_attr(b, "data-nav")
      )
    )
    wait_until(b, settled, timeout = 60, what = paste("the", view, "screen"))
  }

  # A screen reached from the Instance screen lights the Instance link
  # rather than none: a reader on the Performance screen can still see
  # where they are.
  click(b, ".episodic-nav-link[data-view='instance']")
  wait_until(b, settled, timeout = 60, what = "the Instance screen")
  hit <- click(b, ".episodic-instance-card[data-episodic-nav='performance']")
  if (identical(hit, "ok")) {
    record(
      ctx,
      "an instance card opens its screen and keeps the Instance link lit",
      identical(shell_attr(b, "data-view"), "performance") &&
        identical(shell_attr(b, "data-nav"), "instance"),
      sprintf(
        "view=%s nav=%s",
        shell_attr(b, "data-view"),
        shell_attr(b, "data-nav")
      )
    )
    wait_until(b, settled, timeout = 90, what = "the Performance screen")
  } else {
    record(ctx, "an instance card opens its screen", FALSE, hit)
  }

  click(b, ".episodic-nav-link[data-view='clusters']")
  wait_until(b, settled, timeout = 60, what = "the clusters screen")
}

check_pane_survives_navigation <- function(b, ctx) {
  # The single most-reported symptom, and the one no structural test can
  # catch: the pane used to live on an element inside the screen, so
  # every navigation rebuilt it back to the dossier.
  hit <- click(b, ".episodic-pane-tab[data-episodic-pane='assessment']")
  if (!identical(hit, "ok")) {
    record(ctx, "pane survives a navigation", FALSE, hit)
    return(invisible(NULL))
  }
  record(
    ctx, "the segmented control switches pane",
    identical(shell_attr(b, "data-pane"), "assessment"),
    shell_attr(b, "data-pane")
  )

  click(b, ".episodic-nav-link[data-view='archive']")
  wait_until(b, settled, timeout = 60, what = "the Archive screen")
  click(b, ".episodic-nav-link[data-view='clusters']")
  wait_until(b, settled, timeout = 60, what = "the clusters screen")

  record(
    ctx,
    "the pane survives a navigation and back",
    identical(shell_attr(b, "data-pane"), "assessment"),
    sprintf("came back on pane=%s", shell_attr(b, "data-pane"))
  )
  click(b, ".episodic-pane-tab[data-episodic-pane='dossier']")
}

check_escape_closes_rail <- function(b, ctx) {
  click(b, ".episodic-rail-toggle")
  record(
    ctx, "the rail toggle opens the rail",
    identical(shell_attr(b, "data-pane"), "rail"),
    shell_attr(b, "data-pane")
  )
  for (type in c("keyDown", "keyUp")) {
    b$Input$dispatchKeyEvent(
      type = type,
      key = "Escape",
      code = "Escape",
      windowsVirtualKeyCode = 27L,
      nativeVirtualKeyCode = 27L
    )
  }
  Sys.sleep(0.2)
  record(
    ctx, "Escape closes the rail",
    identical(shell_attr(b, "data-pane"), "dossier"),
    shell_attr(b, "data-pane")
  )
}

check_return_is_free <- function(b, ctx) {
  # A screen is built the first time it is looked at and not rebuilt on
  # the way back. Reported as well as asserted: the two numbers say more
  # than the verdict does.
  timed <- function(view) {
    started <- Sys.time()
    click(b, sprintf(".episodic-nav-link[data-view='%s']", view))
    wait_until(b, settled, timeout = 120, what = paste("the", view, "screen"))
    as.numeric(difftime(Sys.time(), started, units = "secs"))
  }
  # Reloaded first: by this point in the pass the Pathogen screen has
  # already been visited once, and timing a second visit against a third
  # measures nothing.
  open_app(b)
  first <- timed("pathogen")
  click(b, ".episodic-nav-link[data-view='clusters']")
  wait_until(b, settled, timeout = 60)
  second <- timed("pathogen")

  record(
    ctx,
    "returning to a screen does not rebuild it",
    second <= max(0.25, first / 3),
    sprintf("first visit %.2fs, return %.2fs", first, second)
  )
}

check_no_js_errors <- function(b, ctx) {
  errs <- js(b, "window.__episodicErrors || []")
  record(
    ctx,
    "no JavaScript errors",
    length(errs) == 0,
    paste(unlist(errs), collapse = " | ")
  )
}

screenshot <- function(b, lang, vp, name) {
  dir <- file.path(shot_dir, lang)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  b$screenshot(
    filename = file.path(dir, sprintf("%s-%s.png", vp$name, name)),
    show = FALSE
  )
}

# --------------------------------------------------------------------- #
# The pass                                                              #
# --------------------------------------------------------------------- #

for (lang in LANGS) {
  message(sprintf("\n== %s ==", lang))
  app_process <- start_app(lang, open_config)
  b <- new_session()

  for (vp in VIEWPORTS) {
    ctx <- sprintf("%s/%s", lang, vp$name)
    message(sprintf("  %s (%dx%d)", vp$name, vp$width, vp$height))
    open_app(b)
    set_viewport(b, vp)

    check_viewport(b, ctx, vp, lang)
    screenshot(b, lang, vp, "clusters")

    check_navigation(b, ctx)
    screenshot(b, lang, vp, "after-navigation")

    if (vp$width < 768) {
      check_pane_survives_navigation(b, ctx)
      screenshot(b, lang, vp, "pane-assessment")
    }
    if (vp$width >= 768 && vp$width < 1200) {
      check_escape_closes_rail(b, ctx)
    }
    if (identical(vp$name, "laptop")) {
      check_return_is_free(b, ctx)
    }
    check_no_js_errors(b, ctx)
  }
  b$close()
}

# The locked screen, with the shipped default rather than the opened
# config: an instance closed to anonymous visitors is what an operator
# gets out of the box, and it has a layout of its own to get right.
message("\n== locked screen (shipped access defaults) ==")
app_process <- start_app("en", demo_files$config)
b <- new_session()
for (vp in VIEWPORTS) {
  ctx <- sprintf("locked/%s", vp$name)
  open_app(b)
  set_viewport(b, vp)
  Sys.sleep(0.3)
  record(
    ctx,
    "the locked screen is shown and no screen is",
    isTRUE(js(b, "!!document.querySelector('.episodic-locked-screen')")) &&
      js(b, "
        Array.prototype.filter.call(
          document.querySelectorAll('.episodic-screen'),
          function (s) { return s.offsetParent !== null; }
        ).length") == 0
  )
  overflow <- js(b, "
    ({ scroll: document.documentElement.scrollWidth, inner: window.innerWidth })")
  record(
    ctx, "no horizontal overflow",
    overflow$scroll <= overflow$inner + 1,
    sprintf("%d vs %d", overflow$scroll, overflow$inner)
  )
  screenshot(b, "locked", vp, "locked")
}
check_no_js_errors(b, "locked")
b$close()

# --------------------------------------------------------------------- #

message("\n== summary ==")
failed <- Filter(function(r) !r$ok, results)
message(sprintf(
  "%d assertions, %d failed. Screenshots in %s",
  length(results),
  length(failed),
  shot_dir
))
if (length(failed) > 0) {
  for (r in failed) {
    message(sprintf("  FAIL %s :: %s - %s", r$context, r$name, r$detail))
  }
  quit(status = 1L, save = "no")
}
message("Every assertion held.")
quit(status = 0L, save = "no")
