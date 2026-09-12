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

# What this file holds is the rule the navigation is built on: there is
# one place each piece of its state lives, and every appearance is
# derived from that place. Most of these assertions are about what is
# *absent* - no class marking a link as current, no inline handler, no
# second copy of a mapping - because that absence is the whole design.

# Not the test-i18n.R constant of the same name: test files load in
# alphabetical order (this one before test-i18n.R), so relying on that
# file's top-level binding here would be order-dependent.
episodic_nav_shipped_langs <- c("nl", "en", "es", "fr", "de", "zh", "hi", "ar")

episodic_nav_css <- function() {
  paste(
    readLines(
      system.file("app/www/episodic.css", package = "EpiSODIC"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

episodic_nav_js <- function() {
  paste(
    readLines(
      system.file("app/www/episodic-nav.js", package = "EpiSODIC"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

# The page's own head content is lifted into its own slot by htmltools,
# so as.character() on the page returns the body alone. Reassembled here.
episodic_nav_page <- function(lang = "en") {
  rendered <- htmltools::renderTags(episodic_app_ui(lang))
  paste(c(rendered$head, rendered$html), collapse = "\n")
}

# ---------------------------------------------------------------------
# One state, one place

test_that("the shell carries the navigation state and nothing else does", {
  html <- as.character(episodic_app_ui("en"))
  expect_true(grepl('class="episodic-shell"', html, fixed = TRUE))
  for (attr in c('data-view="clusters"', 'data-nav="clusters"', 'data-cluster=""')) {
    expect_true(grepl(attr, html, fixed = TRUE), info = attr)
  }
  # `data-access` is absent until the server says otherwise, so the
  # default is the app rather than the wall: a session that may read
  # never sees a locked screen flash past.
  expect_false(grepl("data-access", html, fixed = TRUE))
  # `data-pane` appears nowhere in the markup: it is written at runtime
  # onto the shell and onto nothing else. Held on .episodic-body, as it
  # was, it sat inside a screen and therefore inside markup a render
  # replaces, which is what reset the phone's pane to the dossier on
  # every navigation.
  expect_false(grepl("data-pane=", html, fixed = TRUE))
})

test_that("every screen is written into the page, once each", {
  html <- as.character(episodic_app_ui("en"))
  for (view in episodic_app_views()) {
    expect_equal(
      lengths(regmatches(
        html,
        gregexpr(sprintf('data-screen="%s"', view), html, fixed = TRUE)
      ))[[1]],
      1,
      info = view
    )
  }
  # A screen in the page is a screen Shiny can suspend rather than
  # rebuild, which is what makes a navigation cost nothing.
  expect_equal(
    lengths(regmatches(html, gregexpr("episodic-screen\"", html)))[[1]],
    length(episodic_app_views())
  )
})

test_that("the stylesheet decides what is visible, for every screen and every pane", {
  css <- episodic_nav_css()
  for (view in episodic_app_views()) {
    expect_true(
      grepl(
        sprintf(
          '.episodic-shell[data-view="%s"] .episodic-screen[data-screen="%s"]',
          view,
          view
        ),
        css,
        fixed = TRUE
      ),
      info = view
    )
  }
  for (pane in c("rail", "dossier", "assessment")) {
    expect_true(
      grepl(
        sprintf('.episodic-shell[data-pane="%s"] .episodic-pane-%s', pane, pane),
        css,
        fixed = TRUE
      ),
      info = pane
    )
    expect_true(
      grepl(
        sprintf(
          '.episodic-shell[data-pane="%s"] .episodic-pane-tab[data-episodic-pane="%s"]',
          pane,
          pane
        ),
        css,
        fixed = TRUE
      ),
      info = pane
    )
  }
  for (group in c("clusters", "pathogen", "archive", "instance")) {
    expect_true(
      grepl(
        sprintf(
          '.episodic-shell[data-nav="%s"] .episodic-nav-link[data-view="%s"]',
          group,
          group
        ),
        css,
        fixed = TRUE
      ),
      info = group
    )
  }
})

test_that("nothing toggles a styling class to mark what is current", {
  js <- episodic_nav_js()
  # The whole point: a class is a second copy of state that the
  # stylesheet already derives, and two copies drift. `aria-current` is
  # written for the rail (which a stylesheet cannot derive, since a
  # cluster id is not known when it is written) and as the accessible
  # mirror elsewhere.
  expect_false(grepl("classList.add", js, fixed = TRUE))
  expect_false(grepl("classList.remove", js, fixed = TRUE))
  expect_false(grepl("classList.toggle", js, fixed = TRUE))
  expect_false(grepl('"active"', js, fixed = TRUE))
  expect_false(grepl("'active'", js, fixed = TRUE))
})

test_that("the mapping from screen to nav link exists in R only", {
  js <- episodic_nav_js()
  # R decides it once (episodic_app_nav_group()) and it reaches the
  # browser as `data-nav` on whatever navigates and in the server's own
  # message. A copy here would be a second thing to keep in step.
  for (view in c("streams", "activity", "performance", "info", "settings")) {
    expect_false(grepl(sprintf('"%s"', view), js, fixed = TRUE), info = view)
  }
})

# ---------------------------------------------------------------------
# One listener, no inline handlers

test_that("the navigation layer carries no inline event handler at all", {
  fixture <- data.frame(
    cluster_id = 3L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 2L,
    priority_score = NA_real_,
    first_day = "2025-01-01",
    last_day = "2025-01-05",
    stringsAsFactors = FALSE
  )
  parts <- list(
    nav = as.character(episodic_ui_nav_links(lang = "en")),
    rail = as.character(episodic_ui_rail(fixture, selected_id = NULL, lang = "en")),
    switcher = as.character(episodic_ui_pane_switcher(lang = "en")),
    card = as.character(
      episodic_ui_instance_card("streams", "Streams", "What it watches.", "12 streams")
    ),
    row = as.character(
      episodic_ui_cluster_row(7L, shiny::tags$td("x"), lang = "en")
    ),
    chip = as.character(
      episodic_ui_chip_link("linked", "#123456", 9L, lang = "en")
    ),
    link = as.character(episodic_ui_cluster_link("#9", 9L, lang = "en"))
  )
  for (nm in names(parts)) {
    for (handler in c("onclick", "onkeydown", "onchange")) {
      expect_false(
        grepl(handler, parts[[nm]], fixed = TRUE),
        info = paste(nm, handler)
      )
    }
    # A rail of forty clusters used to carry forty copies of the same
    # two lines of JavaScript, re-sent and re-parsed on every render.
    expect_false(
      grepl("Shiny.setInputValue", parts[[nm]], fixed = TRUE),
      info = nm
    )
  }
})

test_that("everything that navigates says so with one of four data attributes", {
  fixture <- data.frame(
    cluster_id = 3L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 2L,
    priority_score = NA_real_,
    first_day = "2025-01-01",
    last_day = "2025-01-05",
    stringsAsFactors = FALSE
  )
  expect_true(grepl(
    'data-episodic-nav="archive"',
    as.character(episodic_ui_nav_links(lang = "en")),
    fixed = TRUE
  ))
  expect_true(grepl(
    'data-episodic-pane="dossier"',
    as.character(episodic_ui_pane_switcher(lang = "en")),
    fixed = TRUE
  ))
  rail <- as.character(
    episodic_ui_rail(fixture, selected_id = NULL, lang = "en")
  )
  expect_true(grepl('data-episodic-cluster="3"', rail, fixed = TRUE))
  expect_true(grepl('data-episodic-action="rail-open"', rail, fixed = TRUE))
})

test_that("the rail row opens a cluster from a button beside the checkbox, not around it", {
  fixture <- data.frame(
    cluster_id = 3L,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 2L,
    priority_score = NA_real_,
    first_day = "2025-01-01",
    last_day = "2025-01-05",
    stringsAsFactors = FALSE
  )
  user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    stringsAsFactors = FALSE
  )
  html <- as.character(
    episodic_ui_rail(fixture, selected_id = 3L, lang = "en", current_user = user)
  )
  # A form control inside a <button> is invalid, and browsers differ on
  # whether it can be ticked at all - so the checkbox is a sibling of
  # the opener, not a descendant of it.
  expect_true(grepl("episodic-rail-item-open", html, fixed = TRUE))
  checkbox_at <- regexpr("episodic-rail-select", html, fixed = TRUE)
  opener_at <- regexpr("episodic-rail-item-open", html, fixed = TRUE)
  expect_true(checkbox_at > 0)
  expect_true(opener_at > 0)
  expect_lt(checkbox_at, opener_at)
  expect_true(grepl("episodic-rail-item-selectable", html, fixed = TRUE))
  # The row is marked by aria-current alone.
  expect_true(grepl('aria-current="true"', html, fixed = TRUE))
  expect_false(grepl("episodic-rail-item active", html, fixed = TRUE))

  # No checkbox for a reader who cannot record a verdict, and therefore
  # no indent reserved for one.
  plain <- as.character(
    episodic_ui_rail(fixture, selected_id = NULL, lang = "en")
  )
  expect_false(grepl("episodic-rail-select", plain, fixed = TRUE))
  expect_false(grepl("episodic-rail-item-selectable", plain, fixed = TRUE))
})

# ---------------------------------------------------------------------
# Visible always, and reachable by a finger

test_that("the locked screen is shown by server-owned state, not by guessing at an empty output", {
  css <- episodic_nav_css()
  expect_true(grepl(
    '.episodic-shell[data-access="locked"] .episodic-screens',
    css,
    fixed = TRUE
  ))
  expect_true(grepl(
    '.episodic-shell[data-access="locked"] #locked_screen',
    css,
    fixed = TRUE
  ))
  # `:empty` on a Shiny output is a guess about whitespace, and the way
  # that guess fails is by hiding every screen from a reader entitled to
  # all of them. Checked on the declarations, not the raw text: the
  # paragraph above says `:empty` while explaining why the rule doesn't
  # use it, and a comment is not a selector.
  declarations <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
  expect_false(grepl(":empty", declarations, fixed = TRUE))
})

test_that("no media query ever hides the navigation", {
  css <- episodic_nav_css()
  # The bar is four links and it is on screen at every width. Below
  # 768px it takes a row of its own and, if a language's labels ever
  # overran it, wraps to a second - it degrades by wrapping, never by
  # hiding, which is what "visible always" has to mean to be worth
  # anything.
  expect_false(grepl(".episodic-nav {\n    display: none", css, fixed = TRUE))
  expect_false(grepl(".episodic-nav-toggle", css, fixed = TRUE))
  expect_false(grepl("episodic-nav.open", css, fixed = TRUE))
  expect_true(grepl("flex-basis: 100%", css, fixed = TRUE))
})

test_that("every navigation control has a 44px target under the touch breakpoint", {
  css <- episodic_nav_css()
  touch <- substr(
    css,
    regexpr("@media (max-width: 1199px)", css, fixed = TRUE),
    regexpr("/* 768-1199px", css, fixed = TRUE)
  )
  for (sel in c(
    ".episodic-btn",
    ".episodic-picker-btn",
    ".episodic-nav-link",
    ".episodic-rail-item-open"
  )) {
    expect_true(grepl(sel, touch, fixed = TRUE), info = sel)
  }
  expect_equal(
    lengths(regmatches(touch, gregexpr("min-height: 44px", touch)))[[1]] > 0,
    TRUE
  )
  # A press has to look like it landed before the screen has had time to
  # change, or it reads as a press that missed and gets repeated.
  expect_true(grepl("touch-action: manipulation", css, fixed = TRUE))
  expect_true(grepl(".episodic-nav-link:active", css, fixed = TRUE))
  expect_true(grepl(".episodic-pane-tab:active", css, fixed = TRUE))
})

test_that("the panes carry their own geometry, on the output containers", {
  css <- episodic_nav_css()
  html <- as.character(episodic_app_ui("en"))
  # shiny::uiOutput() renders a div of its own, and that div is what
  # .episodic-body lays out. With the geometry one level further in, the
  # dossier's `flex: 1` addresses nothing and no pane scrolls inside
  # itself.
  for (pane in c("rail", "dossier", "assessment")) {
    # shiny::uiOutput(class =) prepends its own "shiny-html-output" token
    # ahead of whatever class is passed, so the two pane classes are the
    # end of the attribute, not the whole of it.
    expect_true(
      grepl(
        sprintf('episodic-pane episodic-pane-%s"', pane),
        html,
        fixed = TRUE
      ),
      info = pane
    )
    expect_true(
      grepl(sprintf(".episodic-pane-%s {", pane), css, fixed = TRUE),
      info = pane
    )
  }
  expect_true(grepl(".episodic-pane-dossier {\n  flex: 1 1 0;", css, fixed = TRUE))
})

test_that("right-to-left is expressed logically, with the one transform named", {
  css <- episodic_nav_css()
  # Up to, but not including, the Right-to-left section itself - that
  # section's own prose names the physical properties it forbids.
  new_rules <- substr(
    css,
    regexpr("Responsive layout", css, fixed = TRUE),
    regexpr("Right-to-left", css, fixed = TRUE) - 1L
  )
  # A margin-left added here is a component that stays stubbornly
  # left-aligned in Arabic while everything around it has moved.
  expect_false(grepl("margin-left", new_rules, fixed = TRUE))
  expect_false(grepl("margin-right", new_rules, fixed = TRUE))
  expect_false(grepl("padding-left", new_rules, fixed = TRUE))
  expect_false(grepl("padding-right", new_rules, fixed = TRUE))
  # translateX names a physical axis and cannot be written logically, so
  # it gets an explicit rule beside the one it inverts.
  expect_true(grepl('[dir="rtl"] .episodic-pane-rail', css, fixed = TRUE))
})

# ---------------------------------------------------------------------
# The Instance screen

test_that("the Instance screen offers the five screens the bar no longer carries", {
  instance <- list(
    counts = list(streams = 12L, runs = 4L, users = 3L, clusters = 9L),
    schema_version = 5L
  )
  html <- as.character(episodic_ui_instance_screen(instance, lang = "en"))
  for (view in c("streams", "activity", "performance", "info")) {
    expect_true(
      grepl(sprintf('data-episodic-nav="%s"', view), html, fixed = TRUE),
      info = view
    )
  }
  # Every card keeps the Instance link lit, so a reader on the
  # Performance screen can still see where they are.
  expect_false(grepl('data-nav="performance"', html, fixed = TRUE))
  expect_true(grepl('data-nav="instance"', html, fixed = TRUE))

  # Settings is admin-only, the same gate the Settings screen re-checks
  # server-side before it renders at all.
  expect_false(grepl('data-episodic-nav="settings"', html, fixed = TRUE))
  admin <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    is_admin = 1L,
    stringsAsFactors = FALSE
  )
  expect_true(grepl(
    'data-episodic-nav="settings"',
    as.character(
      episodic_ui_instance_screen(instance, current_user = admin, lang = "en")
    ),
    fixed = TRUE
  ))
})

test_that("the Performance card deliberately carries no number", {
  instance <- list(
    counts = list(streams = 12L, runs = 4L, users = 3L, clusters = 9L),
    schema_version = 5L
  )
  html <- as.character(episodic_ui_instance_screen(instance, lang = "en"))
  cards <- lengths(regmatches(
    html,
    gregexpr("episodic-instance-card-title", html, fixed = TRUE)
  ))[[1]]
  metas <- lengths(regmatches(
    html,
    gregexpr("episodic-instance-card-meta", html, fixed = TRUE)
  ))[[1]]
  # Four cards, three numbers. Measuring the instance against its
  # epidemiologists' verdicts is a real computation, and running it to
  # fill in a line nobody asked for - every time somebody passes through
  # on the way to Settings - would turn opening this screen into a
  # query.
  expect_equal(cards, 4)
  expect_equal(metas, 3)
})

test_that("the Instance screen renders in every shipped language with no missing key", {
  instance <- list(
    counts = list(streams = 12L, runs = 4L, users = 3L, clusters = 9L),
    schema_version = 5L
  )
  for (lang in episodic_nav_shipped_langs) {
    html <- as.character(episodic_ui_instance_screen(instance, lang = lang))
    # a translation miss falls through to episodic_tr()'s "[[key]]"
    expect_false(grepl("[[", html, fixed = TRUE), info = lang)
    expect_true(
      grepl(episodic_tr("nav.instance", lang = lang), html, fixed = TRUE),
      info = lang
    )
  }
})

test_that("every key the new navigation uses exists in every shipped language", {
  keys <- c(
    "nav.instance",
    "nav.menu_label",
    "pane.dossier",
    "pane.switcher_label",
    "rail.bulk_select",
    "instance.lead",
    "instance.schema_version",
    "instance.card.streams",
    "instance.card.activity",
    "instance.card.performance",
    "instance.card.info",
    "instance.card.settings",
    "unit.run",
    "unit.runs",
    "unit.account",
    "unit.accounts"
  )
  for (lang in episodic_nav_shipped_langs) {
    table <- episodic_i18n_load(lang)
    for (key in keys) {
      expect_true(key %in% names(table), info = paste(lang, key))
      expect_true(nzchar(table[[key]]), info = paste(lang, key))
    }
  }
})

test_that("episodic_app_ui() assembles without error in every shipped language", {
  for (lang in episodic_nav_shipped_langs) {
    expect_s3_class(episodic_app_ui(lang), "shiny.tag.list")
  }
})

test_that("the page loads its navigation from one cached file rather than inline scripts", {
  html <- episodic_nav_page("en")
  expect_true(grepl('src="www/episodic-nav.js"', html, fixed = TRUE))
  # The only script written into the page is the one thing that has to
  # be true before the first paint: an Arabic reader must not see a
  # left-to-right layout while a file loads.
  scripts <- regmatches(html, gregexpr("<script>", html, fixed = TRUE))[[1]]
  expect_length(scripts, 1)
  expect_true(grepl("setAttribute('dir'", html, fixed = TRUE))
  # The font's own origin is warmed before the stylesheet naming it has
  # been parsed, rather than a round trip after.
  expect_true(grepl('rel="preconnect"', html, fixed = TRUE))
})

test_that("a rendered clusters screen carries exactly one of each assessment input", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  fake_user <- data.frame(
    user_id = 1L,
    username = "jdoe",
    full_name = "Jane Doe",
    role = "epidemiologist",
    stringsAsFactors = FALSE
  )
  open <- data.frame(
    cluster_id = env$cluster_id,
    pathogen = "Norovirus",
    level_label = "L1",
    state = "new",
    state_label = "New",
    n_cases = 6L,
    priority_score = 40,
    first_day = "2025-01-01",
    last_day = "2025-01-10",
    stringsAsFactors = FALSE
  )
  html <- paste(
    as.character(episodic_ui_rail(
      open,
      selected_id = env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_dossier(
      env$con,
      env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_assessment_rail(
      env$con,
      env$cluster_id,
      lang = "en",
      current_user = fake_user
    )),
    as.character(episodic_ui_pane_switcher(lang = "en")),
    collapse = "\n"
  )
  for (id in c("assess_verdict", "assess_rationale", "assess_snooze")) {
    expect_equal(
      lengths(regmatches(
        html,
        gregexpr(sprintf('id="%s"', id), html, fixed = TRUE)
      ))[[1]],
      1,
      info = id
    )
  }
})

test_that("the pane label names a cluster the rail does not list", {
  env <- app_read_setup()
  on.exit(DBI::dbDisconnect(env$con))
  label <- as.character(
    episodic_ui_pane_label(env$con, env$cluster_id, lang = "en")
  )
  expect_true(grepl(
    episodic_tr("dossier.cluster_ref", id = env$cluster_id, lang = "en"),
    label,
    fixed = TRUE
  ))
  # An id naming no cluster gets no label rather than an empty one
  # presented as "nothing is open".
  expect_null(episodic_ui_pane_label(env$con, 999999L, lang = "en"))
})
