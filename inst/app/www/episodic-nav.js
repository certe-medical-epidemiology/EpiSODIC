/*
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
*/

/* EpiSODIC dashboard navigation.
 *
 * One rule governs this file, and every design decision in it follows
 * from that rule: navigation state lives in exactly one place - three
 * data attributes on `.episodic-shell` - and every visual consequence is
 * derived from those attributes by the stylesheet. Nothing here adds or
 * removes a styling class, so no highlight can drift out of step with
 * the thing it highlights, and no re-render can lose one: `.episodic-shell`
 * is written once by episodic_app_ui() and Shiny never replaces it.
 *
 *   data-view     which screen is on top       (9 values, see episodic_app_views())
 *   data-nav      which nav link is lit        (4 values, see episodic_app_nav_group())
 *   data-cluster  which cluster is open        (a cluster id, or "")
 *   data-access   whether this session may read (absent, or "locked")
 *
 * The first two and the last are read by CSS. `data-cluster` cannot be -
 * a stylesheet cannot compare one element's attribute against another's
 * - so the rail row's own `aria-current` carries it, written only by
 * setCluster() below and read as `[aria-current="true"]`. It is one
 * attribute serving as both the accessible state and the styling hook,
 * rather than a class kept beside an attribute saying the same thing.
 *
 * Three of the four have exactly two writers: the click, for an answer
 * with no round trip in it, and the server, for the authoritative value.
 * Both go through the same function here, and the server's write lands
 * last, so an optimistic move the server declines to make corrects
 * itself rather than being left behind. `data-access` has one writer,
 * the server, because an access decision has no optimistic half.
 *
 * Every handler is delegated from `document`. Nothing in this file is
 * bound to an element, so nothing has to be re-bound when Shiny replaces
 * one, and no markup anywhere in the app carries an inline `onclick`
 * whose text is re-sent and re-parsed on every render.
 */
(function () {
  "use strict";

  function shell() {
    return document.querySelector(".episodic-shell");
  }

  /* The clusters screen's three panes, in the order the phone-tier
     segmented control shows them. Used only to validate an incoming
     value, so a malformed one leaves the pane where it was rather than
     hiding all three. */
  var PANES = ["rail", "dossier", "assessment"];

  /* --------------------------------------------------------------- *
   * The three writers                                               *
   * --------------------------------------------------------------- */

  /* `group` is which nav link lights up, which is not always the view
     itself: the five instance-level screens are reached from the
     Instance screen rather than from the bar, and light its link. It is
     read off the element that was clicked rather than mapped here,
     because R already decided it once in episodic_app_nav_group() and a
     second copy of that mapping in JavaScript is a second thing to keep
     in step. */
  function setView(view, group) {
    var el = shell();
    if (!el || !view) {
      return;
    }
    el.setAttribute("data-view", view);
    el.setAttribute("data-nav", group || view);
    /* aria-current is the one piece of this the stylesheet cannot
       express, so it is mirrored here rather than derived. The visual
       highlight does not read it - that comes from data-nav - so the two
       cannot disagree about which link is lit even if this loop were
       ever to miss one. */
    var links = document.querySelectorAll(".episodic-nav-link");
    for (var i = 0; i < links.length; i++) {
      if (links[i].getAttribute("data-view") === (group || view)) {
        links[i].setAttribute("aria-current", "page");
      } else {
        links[i].removeAttribute("aria-current");
      }
    }
  }

  function setPane(pane) {
    var el = shell();
    if (!el || PANES.indexOf(pane) < 0) {
      return;
    }
    el.setAttribute("data-pane", pane);
    /* aria-current, not aria-selected or aria-pressed: below 768px these
       three buttons are a set and one of them is the current member of
       it, which is what aria-current means. They are not tabs (there are
       no tabpanels above 768px, where all three panes are on screen at
       once) and they are not toggles. The same attribute marks the
       current nav link and the current rail row, so the whole navigation
       layer says "current" one way. */
    var tabs = document.querySelectorAll(".episodic-pane-tab");
    for (var i = 0; i < tabs.length; i++) {
      if (tabs[i].getAttribute("data-episodic-pane") === pane) {
        tabs[i].setAttribute("aria-current", "true");
      } else {
        tabs[i].removeAttribute("aria-current");
      }
    }
  }

  /* An id that names no row in the rail marks no row, which is the
     honest answer for a closed cluster opened from the Pathogen screen
     or a `?cluster=` link: it is open, and it is not in this list. What
     names the cluster on the phone is output$pane_label, from the
     server's own selection, not this. */
  function setCluster(id) {
    var el = shell();
    if (!el) {
      return;
    }
    var key = id === null || id === undefined ? "" : String(id);
    el.setAttribute("data-cluster", key);
    var rows = document.querySelectorAll(".episodic-rail-item");
    for (var i = 0; i < rows.length; i++) {
      if (key !== "" && rows[i].getAttribute("data-cluster-id") === key) {
        rows[i].setAttribute("aria-current", "true");
      } else {
        rows[i].removeAttribute("aria-current");
      }
    }
  }

  /* Whether this session may read anything at all. Server-owned, like
     the view: `episodic_app_access_granted()` decides it and nothing
     here may second-guess that. It is an attribute rather than the
     stylesheet noticing that the locked-screen output came back empty -
     `:empty` on a Shiny output is a guess about whitespace, and the way
     that guess fails is by hiding every screen from a reader who is
     entitled to all of them.

     Absent until the server says otherwise, so a session that may read
     never sees a locked screen flash past, and a session that may not
     sees empty screens rather than data for the one round trip it takes
     to arrive. */
  function setAccess(state) {
    var el = shell();
    if (!el) {
      return;
    }
    if (state === "locked") {
      el.setAttribute("data-access", "locked");
    } else {
      el.removeAttribute("data-access");
    }
  }

  /* Re-marks the rail from the shell's own attribute. output$rail_pane
     re-renders whenever the open-cluster list changes (a run, a closure,
     a sign-in), which replaces every row and with them the aria-current
     this file wrote; the selection itself has not changed, and the
     server has no reason to re-send it. */
  function remarkRail() {
    var el = shell();
    if (!el) {
      return;
    }
    setCluster(el.getAttribute("data-cluster") || "");
  }

  /* --------------------------------------------------------------- *
   * Talking to Shiny                                                *
   * --------------------------------------------------------------- */

  function ready() {
    return typeof window.Shiny !== "undefined" && !!window.Shiny.setInputValue;
  }

  /* The view is state, not an event: it is where the reader is, it is
     read back by every screen, and re-selecting the screen already shown
     is not a request to do anything. So it is set as an ordinary input
     value, which also means Shiny sends nothing at all for a click on
     the current screen. */
  function tellShinyView(view) {
    if (ready()) {
      window.Shiny.setInputValue("nav_view", view);
    }
  }

  function tellShinyEvent(name, value) {
    if (ready()) {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  /* The bulk-assessment bar counts the rail's own checked boxes rather
     than holding a selection of its own, so that checking a box never
     costs a round trip and never re-renders the rail underneath the
     reader's scroll position. */
  function bulkUpdate() {
    var bar = document.getElementById("episodic-bulk-bar");
    if (!bar) {
      return;
    }
    var checked = document.querySelectorAll(".episodic-rail-select:checked");
    bar.hidden = checked.length === 0;
    var count = document.getElementById("episodic-bulk-count");
    if (count) {
      count.textContent = String(checked.length);
    }
  }

  function checkedClusterIds() {
    var checked = document.querySelectorAll(".episodic-rail-select:checked");
    var ids = [];
    for (var i = 0; i < checked.length; i++) {
      var id = parseInt(checked[i].value, 10);
      if (!isNaN(id)) {
        ids.push(id);
      }
    }
    return ids;
  }

  /* --------------------------------------------------------------- *
   * The named actions                                               *
   * --------------------------------------------------------------- */

  var ACTIONS = {
    "rail-open": function () {
      var box = document.querySelector(".episodic-rail-open-input");
      if (!box) {
        return;
      }
      var id = parseInt(box.value, 10);
      if (isNaN(id)) {
        return;
      }
      /* An event, not a value: the same number typed twice is two
         requests, and the second one is owed the same answer as the
         first. The server decides whether it resolves. */
      tellShinyEvent("rail_open_cluster", id);
    },
    "bulk-apply": function () {
      var ids = checkedClusterIds();
      if (ids.length === 0) {
        return;
      }
      var verdict = document.getElementById("bulk_assess_verdict");
      var rationale = document.getElementById("bulk_assess_rationale");
      tellShinyEvent("bulk_assess_submit", {
        cluster_ids: ids,
        verdict: verdict ? verdict.value : "",
        rationale: rationale ? rationale.value : ""
      });
    },
    "bulk-clear": function () {
      var boxes = document.querySelectorAll(".episodic-rail-select");
      for (var i = 0; i < boxes.length; i++) {
        boxes[i].checked = false;
      }
      bulkUpdate();
    }
  };

  /* --------------------------------------------------------------- *
   * One delegated listener                                          *
   * --------------------------------------------------------------- */

  /* Element.closest() from the event target, so a click on a label, a
     chip or an italicised taxon inside a rail row is a click on the row,
     which is what a reader means by it. */
  function target(ev, selector) {
    var node = ev.target;
    if (!node || !node.closest) {
      return null;
    }
    return node.closest(selector);
  }

  function handle(ev) {
    var nav = target(ev, "[data-episodic-nav]");
    if (nav) {
      ev.preventDefault();
      var view = nav.getAttribute("data-episodic-nav");
      setView(view, nav.getAttribute("data-nav") || view);
      tellShinyView(view);
      return;
    }

    var pane = target(ev, "[data-episodic-pane]");
    if (pane) {
      ev.preventDefault();
      setPane(pane.getAttribute("data-episodic-pane"));
      return;
    }

    var action = target(ev, "[data-episodic-action]");
    if (action) {
      ev.preventDefault();
      var fn = ACTIONS[action.getAttribute("data-episodic-action")];
      if (fn) {
        fn();
      }
      return;
    }

    /* A checkbox inside a rail row selects clusters for the bulk bar; it
       is not a request to open the row it sits in. Checked before the
       row itself, rather than by stopping propagation at the checkbox,
       so the rule lives with the row that needs it. */
    if (target(ev, ".episodic-rail-select")) {
      bulkUpdate();
      return;
    }

    var opener = target(ev, "[data-episodic-cluster]");
    if (opener) {
      ev.preventDefault();
      var id = parseInt(opener.getAttribute("data-episodic-cluster"), 10);
      if (isNaN(id)) {
        return;
      }
      /* Optimistic: the highlight and the pane move now. The server
         answers with the selection it actually made, through
         episodic_cluster below, which is what puts this right when the
         id names a cluster since merged away. */
      setCluster(id);
      setPane("dossier");
      tellShinyEvent("open_cluster", id);
    }
  }

  /* Enter and Space on anything the click handler above would act on.
     Buttons and links get this from the browser; the chips and table
     cells that open a cluster are spans with role="link", and a link a
     keyboard user cannot follow is not a link. */
  function handleKey(ev) {
    if (ev.key !== "Enter" && ev.key !== " " && ev.key !== "Spacebar") {
      return;
    }
    var el = ev.target;
    if (!el || !el.closest) {
      return;
    }
    if (el.tagName === "BUTTON" || el.tagName === "A" || el.tagName === "INPUT") {
      return;
    }
    if (
      el.closest(
        "[data-episodic-nav],[data-episodic-pane],[data-episodic-action],[data-episodic-cluster]"
      )
    ) {
      ev.preventDefault();
      handle(ev);
    }
  }

  /* Escape closes the off-canvas rail at the tier where it is an
     overlay. Harmless at every other width: data-pane is inert above
     1200px, and below 768px the rail is a pane of its own rather than
     something covering the screen, where returning to the dossier is
     what the segmented control's middle segment does anyway. */
  function handleEscape(ev) {
    if (ev.key !== "Escape") {
      return;
    }
    var el = shell();
    if (el && el.getAttribute("data-pane") === "rail") {
      setPane("dossier");
    }
  }

  document.addEventListener("click", handle);
  document.addEventListener("keydown", handleKey);
  document.addEventListener("keydown", handleEscape);
  document.addEventListener("change", function (ev) {
    if (ev.target && ev.target.classList &&
        ev.target.classList.contains("episodic-rail-select")) {
      bulkUpdate();
    }
  });

  /* Enter in the rail's open-by-number box. The box carries no id, so
     Shiny does not bind it as an input and does not round-trip a number
     nothing reads until Open is pressed. */
  document.addEventListener("keydown", function (ev) {
    if (ev.key !== "Enter" || !ev.target || !ev.target.classList) {
      return;
    }
    if (ev.target.classList.contains("episodic-rail-open-input")) {
      ev.preventDefault();
      ACTIONS["rail-open"]();
    }
  });

  /* --------------------------------------------------------------- *
   * The server's side of each of the three                          *
   * --------------------------------------------------------------- */

  function register() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {
      return;
    }
    window.Shiny.addCustomMessageHandler("episodic_view", function (msg) {
      setView(msg.view, msg.nav);
    });
    window.Shiny.addCustomMessageHandler("episodic_pane", function (msg) {
      setPane(msg.pane);
    });
    window.Shiny.addCustomMessageHandler("episodic_cluster", function (msg) {
      setCluster(msg.cluster);
    });
    window.Shiny.addCustomMessageHandler("episodic_access", function (msg) {
      setAccess(msg.access);
    });
    /* The rail's rows are replaced whenever its list changes; the
       selection they mark is not. Through jQuery rather than
       addEventListener, and named explicitly rather than through the `$`
       alias: Shiny raises shiny:value with jQuery's own trigger(), which
       a native listener never sees. */
    if (window.jQuery) {
      window.jQuery(document).on("shiny:value", function (ev) {
        if (ev.name === "rail_pane") {
          remarkRail();
          bulkUpdate();
        }
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", register);
  } else {
    register();
  }
})();
