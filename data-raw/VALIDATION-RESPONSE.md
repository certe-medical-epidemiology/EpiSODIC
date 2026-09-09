# Response to the detection-validation brief

Written for whoever picks up PR #45 (`claude/app-review-shipping-mz3b7t`),
and for the maintainer deciding what merges in what order. It answers
`data-raw/VALIDATION-BRIEF.md`.

The work is PR #49, branch `claude/detection-validation-harness`, branched
from #45's tip because it needs #45's fixes to run at all.

---

## 1. Where things stand

| | |
|---|---|
| Task A, the validation harness | done, tested, and a full 20-seed study run |
| Task B, MariaDB in CI | done, and it found something - see §3, which corrects a claim I made too strongly |
| `devtools::test()` | 7378 pass, 0 fail, 10 skips (7 of them the new MariaDB live tests, which skip without a DSN) |
| the same suite against a live MariaDB | 7387 pass, 0 fail, 3 skips, all environmental |
| `R CMD check --no-manual` | 1 NOTE, which is yours, not mine - see §4 |
| `document()`, `styler::style_pkg()` | no-ops |

Issues #46, #47 and #48 were not touched.

---

## 2. Task A: what exists now

**The ground truth is data.** `episodic_synthetic_cases()` returns what it
injected - one row per outbreak and the case-level membership keyed on
`source_key` - read back with `episodic_synthetic_ground_truth()`. The
`PT-OUTBREAK-*` patient keys still exist and nothing measures against
them. Two new generator arguments: `outbreaks` (which of the six, or none,
for a negative control) and `outbreak_offsets` (days back from `end_date`,
per outbreak, so a prospective evaluation can disperse them across the
window instead of having all six in its last eight weeks).

**The harness** is `episodic_validate_detection()`: generate, replay week
by week handing each run only the cases sampled by that date, and match
clusters to outbreaks **on case sets, in both directions**. Also
`episodic_validate_comparator()` (a naive same-place rule and a 2-SD
Shewhart limit, through the same metrics) and
`episodic_validate_rethreshold()` (re-match a finished replay at other
thresholds, so a sweep costs arithmetic rather than another replay).

**`<detector>.enabled`** was added to each detector's config section,
shipped `true`, because drop-one needs a detector switched off and no
existing knob does that without a hack. It is configuration rather than an
argument to `episodic_run_cron()` so that `config_hash` records which
detectors ran. **This changes `config_hash` for every existing instance**:
the resolved configuration has four more keys. No shipped default *value*
was touched.

**The study** is `data-raw/validation/run_study.R`. Twenty seeds by
twenty-six weekly runs on four years of history, a negative control,
drop-one for each of the four detectors, five comparator runs, a six-point
operating-point sweep and a 5x5 threshold sweep. About three hours. It
writes CSV to `data-raw/validation/results/` with the package version,
`config_hash`, seeds, runs and stream-weeks on every row. `data-raw/` is in
`.Rbuildignore`, so none of it ships, none of it runs under `R CMD check`,
and none of it is reachable from an installed EpiSODIC.

### What it found

Main scenario, 20 seeds x 26 weekly runs, 2026-01-04 to 2026-06-28,
468,710 stream-weeks, config `9b97cc3f4a58`:

| | |
|---|---|
| Sensitivity | 119/120 = **0.99** (0.95-1.00) |
| Per shape | RARE, WARD, LTC, PS, WAVE 20/20; PROP 19/20 |
| PPV | 156/199 = **0.78** (0.72-0.84) |
| False alarms | 43/468,710 = **9.2 x 10<sup>-5</sup> per stream-week** (6.6-12.4) |
| Median delay from first case | **6 days**; from the third, 5; Kaplan-Meier median 5 |
| Fragmentation | **1** - no outbreak was split across dossiers |
| Priority-score AUC | **0.87** (IQR 0.79-0.96); median 58 true positive against 47 false alarm |
| Negative control | 32 alarms over 466,037 stream-weeks = **6.9 x 10<sup>-5</sup> per stream-week** |

Attribution, per shape, over 20 seeds: the regional wave was found first by
Farrington 18/20 and by Farrington+MEM 2/20 - the diffuse signal no rule
can see. The nursing-home outbreak went to `same_place` 15/20 and
Farrington 5/20. The propagated outbreak went to `same_place` 19/19.

### What MEM is doing, which is the study's most interesting result

Dropping `mem` costs no sensitivity at all and removes 14 of the 43 false
alarms. The tempting reading is that this generator gives MEM nothing to
find. The cluster table says otherwise:

- MEM fired on **29 of 199 clusters**.
- **13 were raised by MEM alone, and all 13 are false alarms** - every one
  `pathogen_region`, every one RSV (5) or Influenza A (8), across 8 of the
  20 seeds.
- The other 16 are `farrington+mem`, all Influenza A, 14 of them true
  positives: the seeded wave, which Farrington found as well.

MEM is doing exactly what MEM is for - detecting the onset of the epidemic
season in seasonal respiratory viruses, on four years of history that
contains precisely that. The generator is not at fault.

Where the signal goes is. A seasonal onset is not an outbreak; an
epidemiologist shown one would classify it `expected_variation`. EpiSODIC
routes it into the same queue as aberration signals, so every MEM-only
detection is a dossier raised about the arrival of winter.

That is a design question rather than a threshold to nudge: MEM's output
may belong on the Pathogen screen as seasonal context, or `mem_applicable`
may need to be narrower, or the pre-epidemic threshold may need
configuring. It wants its own issue, and it is not something a validation
PR should settle by tuning - which is the one thing the brief is most
explicit about.

Three results are unflattering and are reported as they are:

- **Only 25% were detected before their own peak**, and a median of 13% of
  an outbreak's cases were still to come at the moment of detection. That
  is the honest ceiling on what earlier detection could have prevented.
- **Lattice suppression never fired.** PPV before and after suppression is
  identical, because fragmentation was 1 everywhere: there was never a
  redundant parent to suppress. On this data the mechanism has nothing to
  do. That is not evidence it is wrong, but it is evidence this generator
  cannot exercise it - and unlike the MEM result above, this one really is
  a gap in the generator.
- **The propagated outbreak is the only shape ever missed** (19/20), and it
  is found by `same_place`, not by Farrington. See §4 on which channel it
  is supposed to exercise.

---

## 3. Task B: a correction to something I claimed too strongly

I wrote in PR #49 that "nobody could ever have created an EpiSODIC
database on MariaDB". The maintainer replied that they had run EpiSODIC
against a database at work, pre-#45, and that setting it up worked. They
are right that the claim as written is wrong. Here is what is actually
true, measured rather than reasoned:

Every foreign key in `inst/sql/schema.sql` is an **inline, column-level**
`REFERENCES`. There is not one table-level `FOREIGN KEY` clause in the
file. Servers disagree about what that means:

| Server | `episodic_db_create()` before #49 | Foreign keys actually created | Orphan row accepted |
|---|---|---|---|
| MariaDB 10.11 | **fails**, errno 150 on the first statement | - | - |
| MariaDB 11.8 | **fails**, errno 150 on the first statement | - | - |
| MySQL 8.4 | **succeeds** | **0** | **yes** |

MariaDB honours inline column-level references, so it refuses
`episodic_stream` outright: the table references `episodic_institution`,
which the file declares eighty lines later. MySQL parses inline references
and discards them, so it creates every table and not one constraint.

Two things follow.

**The failure is not #45's, and not new.** The same experiment against the
schema as of `main` (783116e), pre-#45, fails identically on MariaDB
10.11. The ordering has been wrong for the life of the package.

**So a working pre-#45 instance was not MariaDB with foreign keys.**

Confirmed against the maintainer's own production instance, which is
MySQL: the schema holds five foreign keys, all of them on the `brmo_*`
tables of another application sharing it, and **not one on any
`episodic_*` table**. Same server, same schema, other tables constrained.
That pins the cause on the inline column-level `REFERENCES` beyond any
question of server configuration.

Scoped to EpiSODIC's own tables, which is what to ask on a shared schema:

```sql
SELECT COUNT(*) FROM information_schema.REFERENTIAL_CONSTRAINTS
 WHERE CONSTRAINT_SCHEMA = DATABASE()
   AND TABLE_NAME LIKE 'episodic\_%';
```

Zero there means the constraints in the schema file were never created.
On the production instance it is zero.

### What #49 fixed, and what it did not

`episodic_db_apply_schema()` now declares the schema with
`FOREIGN_KEY_CHECKS = 0` and restores it afterwards, including on the
error path. On MariaDB that turns a total failure into a working database
with real, enforced constraints - the live test inserts an orphan row
immediately after creation and requires it to be refused.

**On MySQL it changes nothing.** `FOREIGN_KEY_CHECKS` is irrelevant there,
because MySQL is not failing to create the constraints, it is declining to
parse them into existence. A MySQL instance still gets 24 tables and 0
foreign keys, silently, and `episodic_db_dsn_mysql()` still advertises
MySQL as supported.

### Fixed, once the maintainer confirmed nothing is deployed

The mariadb dialect now moves every inline reference to a **table-level**
`FOREIGN KEY (col) REFERENCES tbl(col)` clause, which both servers honour.
It is **derived from the schema**, not listed beside it: a list is a second
place to forget a foreign key, and forgetting one is silent on MySQL. A
reference added to `schema.sql` tomorrow is converted tomorrow, and
`test-schema_mariadb.R` asserts that the number of clauses generated equals
the number of references in the file, that none is left inline, and that
every target is a table the schema declares. That is the answer to Task B
item 3: the part that could silently drift is now derived rather than
maintained.

Measured, after the change:

| Server | Tables | Foreign keys | Orphan row |
|---|---|---|---|
| MariaDB 10.11 | 24 | **37** | refused |
| MariaDB 11.8 | 24 | **37** | refused |
| MySQL 8.4 | 24 | **37** | refused |

Thirty-seven is exactly the number of inline references in
`inst/sql/schema.sql`. The live test asserts the count against
`information_schema.REFERENTIAL_CONSTRAINTS` rather than merely asserting
that some constraint exists, because a schema that lost half of them looks
exactly like one that kept them until something writes an orphan.

The SQLite path is untouched. SQLite honours inline references, so
`schema.sql` keeps them and rewriting a schema that already works would be
a change with only risk in it.

**A MariaDB or MySQL database created before this has no constraints and
cannot gain them without being recreated.** No migration is offered,
because there is nothing deployed to migrate: the maintainer has confirmed
the one test instance can be dropped. If that ever stops being true, the
migration has to find the orphan rows first and name them, rather than
failing on the first `ALTER TABLE` with an errno and no list - and any
orphans it finds are themselves a finding, since the SQLite path would
never have allowed them.

### A third defect, found by running the live tests against MySQL

`episodic_db_runs()` bound `limit` to `LIMIT ?` without `as.integer()`,
which its three sibling functions all do. `limit = 200` is a double in R,
and MySQL's prepared-statement protocol refuses a double there:
`Incorrect arguments to mysqld_stmt_execute`. The Activity screen worked on
SQLite and on MariaDB and failed on MySQL, which is the dialect the only
real instance runs.

### What is in CI now

`.github/workflows/mariadb.yaml` runs the whole suite against **both**
`mariadb:11` and `mysql:8` service containers, one Linux job each, and
fails the build if the live tests skipped themselves - a green job that
proved nothing is the failure the workflow exists to prevent. Testing one
server is what hid the inline-reference behaviour for the life of the
package: the two servers disagree, and only one of them says so out loud.

---

## 3b. A second defect, found by the same route, fixed

The production instance shares its schema with another application's
`brmo_*` tables. That shape breaks a different thing.

`episodic_db_exists()` decides which branch `episodic_run_cron()` takes -
open an existing database, or create one. For a `mysql://` DSN it asks
whether the schema holds **any** tables:

```r
length(DBI::dbListTables(con)) > 0
```

On a shared schema that is `TRUE` because of somebody else's tables, even
when no EpiSODIC table has ever been created. Measured, against MySQL 8.4,
on a schema holding one foreign table and nothing else:

1. `episodic_db_exists()` returns `TRUE`.
2. `episodic_run_cron()` therefore calls `episodic_db_connect()` instead of
   `episodic_db_create()`, and refuses: *"This database carries no schema
   version, so it was created by EpiSODIC 0.12.x or earlier. Run
   `episodic_db_migrate()` on it once to bring it up to date."*
3. That advice is wrong, and following it is worse.
   `episodic_db_migrate()` adopts the schema at version 1, applies
   migration 2, and reports *"Database is at schema version 2."*
4. The schema now holds **2 of the 24 EpiSODIC tables**
   (`episodic_schema_version` and `episodic_app_login_failure`) and claims
   to be current. `episodic_db_connect()` opens it happily from then on,
   because the version gate passes.

An operator following the instruction the software gave them ends up with
a database that says it is up to date and is missing 22 tables.

The fix is small and in two parts:

- `episodic_db_exists()` should ask whether *EpiSODIC's* tables exist, not
  whether any do: `intersect(DBI::dbListTables(con), episodic_db_schema_tables())`.
  Both `episodic_db_create(overwrite = TRUE)` and `episodic_db_truncate()`
  already scope themselves that way, so neither can touch a co-tenant's
  tables; this is the one place that does not.
- `episodic_db_migrate()` should refuse to adopt a database at version 1
  unless the core tables are actually present. "No version table" currently
  means "created by 0.12.x or earlier"; it can equally mean "never created
  at all", and those need different answers.

Both parts are done, at the maintainer's instruction, in #49.
`tests/testthat/test-shared_schema.R` covers the migration guard, and
`test-mariadb_live.R` covers the routing against a real server with a
co-tenant table in the schema.

---

## 4. Things that belong to #45, which I have not changed

**`dplyr` is in `Imports` and no longer used.** #45 removed the last
`dplyr::case_when()` (it was in `R/lattice_enumerate.R`) and left the
declaration, so `R CMD check` now reports `Namespace in Imports field not
imported from: 'dplyr'`. It is the only NOTE the check produces. One line
in `DESCRIPTION`. I left it alone because editing `DESCRIPTION` on a branch
stacked on yours only invites a conflict.

**The propagated outbreak's design channel.** The brief's table says
Farrington/Rt. The generator's own roxygen says `same_place`, Rt. I set
`expected_channel = "same_place"`, following the code's documentation over
the brief, and the study confirms it empirically: `same_place` found it
first in 19 of 19 seeds where it was found at all. The brief's table should
be corrected, or the roxygen should, but not both ways.

**One thing I did change, since approved by the maintainer.** With
`EPISODIC_PC_PROVINCE_MAP` unset, a run told the operator that "the shipped
Northern Netherlands demo ranges are in use and match only Dutch 7xxx-9xxx
postcodes". That fallback is exactly what #45 removed, so the message had
become false: it tells an operator their instance is doing something it no
longer does. I rewrote the message and nothing else. It is one hunk in
`R/lattice_enumerate.R`, and the maintainer has confirmed it should stay.

---

## 5. A defect the harness work uncovered, fixed in #49

The geography was resolved from two places inside one run.
`episodic_lattice_enumerate()` named L5 from the run's own configuration
and L3 from `EPISODIC_CONFIG`; `episodic_cases_for_stream()` and
`episodic_db_cases_for_stream_id()` then tested case membership against
whatever `EPISODIC_CONFIG` said.

Where the two disagree - which is what `episodic_run_cron(episodic_config_path = ...)`
is for - every geographic stream matched no case however many arrived, so
Farrington and MEM had nothing to run on at L3, L4 or L5, and any cluster
opened there was written with zero cases linked to it. Nothing said so.

The resolved geography is now computed once per run and passed through
enumeration, stream membership and reconciliation; the default still
resolves `EPISODIC_CONFIG`, which is correct on the dashboard side.
`tests/testthat/test-geography_consistency.R` covers it.

This matters for #45 to know about because it is the mechanism #45
introduced (`config$geography`) that made the inconsistency reachable. It
was latent before: with geography hardcoded, the two sources could not
disagree.

Before the fix, the harness's entire statistical half found nothing: zero
Farrington detections across twenty-six weeks and nine hundred streams.
After it, the regional wave is found four days after its first case.

---

## 6. Branch topology, and merge order

```
main ── 783116e
          └── claude/app-review-shipping-mz3b7t   PR #45  (14 commits)
                    └── claude/detection-validation-harness  PR #49  (16 commits)
```

#49 is branched from #45's tip, so its diff against `main` contains #45's
commits as well. Nothing in #49 rewrites, reverts or reorders anything in
#45.

**Merge #45 first.** Once it lands, #49's diff against `main` reduces to
its own sixteen commits and can be reviewed on its own.

**#45 is in the lead for its own diff.** If #45 gains further commits, #49
must rebase onto them, not the other way round. The files both touch are
`R/run_cron.R`, `R/reconcile.R`, `R/lattice_enumerate.R`, `R/db_read.R`,
`R/schema_migrate.R`, `R/cases_synthetic.R`, `inst/config/episodic_default_config.yaml`,
`NEWS.md`, `CLAUDE.md` and `_pkgdown.yml`, so a rebase is likely to want
attention in `NEWS.md` and `CLAUDE.md` in particular, where both branches
append.

If #45 needs more work before it can merge, the alternative is to retarget
#49's base branch to `claude/app-review-shipping-mz3b7t` on GitHub, which
makes it reviewable immediately as a stacked PR. That is a one-click change
and does not require rebasing anything.

---

## 7. What is not done

- **A migration that adds foreign keys to an existing MySQL/MariaDB
  database.** Not needed today, since nothing is deployed and the one test
  instance can be dropped and recreated. It becomes needed the first time
  an instance holds data worth keeping.
- **A MySQL job in CI.** Worth adding, but not before the above, because it
  would pass today while creating no constraints.
- **Issues #46, #47, #48**, untouched, as the brief instructed. Note for
  #46: `R/cases_synthetic.R` gained the ground-truth machinery and two
  arguments in #49, so #46's review of that file should be done against
  #49's version, not `main`'s.
- **Nothing was tuned.** No value in `inst/config/episodic_default_config.yaml`
  was changed. The operating-point sweep is a curve for the paper, not a
  proposal to move the shipped defaults; if the sweep suggests a better
  operating point, that is a separate conversation with the maintainer, and
  making it in the same breath as the evaluation would be fitting the
  evaluation to the answer.
