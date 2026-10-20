# Plugins

EpiSODIC supports plugins: small add-ons, written by other developers or
by yourself, that let EpiSODIC work with the rest of your world. Every
laboratory and public health service has its own systems, its own
partners and its own way of working, and no single package can know them
all. A plugin closes that gap without anyone having to change EpiSODIC
itself.

## What a plugin can do

A plugin can put a button on an outbreak or epidemic dossier, or quietly
do its work after every detection run. A few examples of what that makes
possible:

- **Share an outbreak with a regional platform** in one click, straight
  from its dossier.
- **Open a ticket** in your organisation’s service desk for every
  outbreak that needs follow-up.
- **Notify the public health service** in the format their own system
  expects.
- **Post a summary of each night’s run** to a team chat or an internal
  dashboard.
- **Copy figures to a data warehouse** for your own reporting.

Plugins can be written in R, Python or Bash, so whoever in your team
knows any of these can make one. And because a plugin only uses what
EpiSODIC has already found, it never changes what EpiSODIC detects.

EpiSODIC ships with one ready-made plugin, which sends an outbreak’s
summary to a web address of your choice. It is a good first plugin to
try, and a good template for a plugin of your own:

``` r

file.show(system.file("plugins", "share-outbreak-http.yaml", package = "EpiSODIC"))
```

## Using plugins

### Installing and switching on

Everything happens on the **Plugins** screen, which an administrator
reaches from its card on the Instance screen.

1.  **Install** a plugin by pasting its address (a file page on GitHub
    will do) or the path of a file on the server. EpiSODIC checks the
    file before saving it, and tells you plainly if something is wrong
    with it.
2.  **Read what it asks for.** The screen lists what each plugin
    receives, which settings it reads and where it sends data.
3.  **Enable** it. EpiSODIC shows once more what you are about to allow,
    and the plugin is ready.

From then on, a button plugin appears on the dossiers it belongs to, for
every epidemiologist. A plugin that runs after each detection run simply
starts doing its work.

Disabling, updating and uninstalling are one click each on the same
screen. An update that asks for more than you allowed before waits for
your approval rather than quietly taking it.

### What you see

Each plugin on the Plugins screen has a status:

- **Enabled**: it is switched on and works.
- **Disabled**: installed, but switched off.
- **Needs approval**: a newer version asks for more than you allowed.
  Enable it again once you agree.
- **Unavailable**: something is missing, such as an R package or a
  program it needs, and the screen says what.
- **File missing**: it is switched on, but its file is gone. Nothing
  else is affected; the plugin simply does not run.

The same screen keeps a record of every time a plugin ran: who started
it, on which outbreak, and how it went.

### Pushing a plugin’s button

On a dossier, a plugin’s button does what its label says. If the plugin
sends the outbreak’s case list out of EpiSODIC, it first asks you to
confirm and names where the data goes. Afterwards you see the plugin’s
message, often with a link to the result, such as the new ticket or the
shared outbreak.

Only epidemiologists see plugin buttons; viewers and visitors who have
not signed in do not.

### Keeping it safe

A plugin runs its own code on your server, so treat installing one the
way you would treat installing any software:

- Install plugins only from sources you trust, and have someone read a
  plugin before you enable it.
- EpiSODIC gives each plugin only what it asked for and you allowed. It
  never gives a plugin EpiSODIC’s own passwords or database access.
- Every run is recorded, including what was sent and where.
- Sending case-level data to another organisation is processing of
  personal data under the GDPR, plugin or not, so it needs the usual
  legal basis and agreement.

### Working from a script

Everything the Plugins screen does is also available from R, for an
administrator who prefers scripts:

``` r

episodic_plugin_install("https://github.com/org/plugins/blob/v1.0.0/share-regional.yaml")
episodic_plugin_list()
episodic_plugin_enable("share-regional")
episodic_plugin_run("share-regional", cluster_id = 12)
episodic_plugin_disable("share-regional")
episodic_plugin_uninstall("share-regional")
```

Plugins are kept in a folder: the one named by `EPISODIC_PLUGIN_DIR`, or
otherwise a `plugins` folder beside your configuration file. Which
plugins are switched on is written in that configuration file, under
`plugins`, so it is visible and editable in the same place as all your
other settings.

## Developing a plugin

A plugin is one YAML file. It says what the plugin is, what it needs,
and which steps it runs. Here is a complete one:

``` yaml
name: Share outbreak with a regional platform
version: 1.0.0
api: 1
trigger: button
placement: outbreak
label:
  en: Share with regional platform
  nl: Delen met regionaal platform
permissions: [cluster_summary]
secrets: [SHARE_ENDPOINT_URL, SHARE_ENDPOINT_TOKEN]
requires:
  packages: [httr2, jsonlite]
timeout: 60
steps:
  - name: Send the outbreak
    shell: r
    run: |
      payload <- jsonlite::read_json(Sys.getenv("EPISODIC_PAYLOAD"))
      # ... send it ...
      cat("message=Shared ", payload$cluster$label, "\n",
          sep = "", file = Sys.getenv("EPISODIC_RESULT"))
```

The file’s name is the plugin’s id: `share-outbreak-http.yaml` is the
plugin `share-outbreak-http`. An id uses lowercase letters, digits and
hyphens.

The easiest way to start is to copy the shipped plugin, give it a name
of its own and change its steps. Install it from its path on the server,
enable it, and push its button.

### Every key

EpiSODIC checks every key. A key it does not know is reported rather
than ignored, so a typo shows up on the Plugins screen straight away.

| Key | Required | What it holds |
|----|----|----|
| `name` | yes | The plugin’s name, as the Plugins screen shows it |
| `version` | yes | The plugin’s own version |
| `api` | yes | The plugin API version it was written for; this EpiSODIC provides `1` |
| `trigger` | yes | `button`, or `run_committed` to run after every detection run |
| `steps` | yes | The steps it runs, in order |
| `placement` | for a button | `outbreak` or `epidemic`: the dossier the button is on |
| `label` | for a button | The button’s text per language; `en` is required and used when a language is missing |
| `description` | no | A description per language, with `en` |
| `permissions` | no | The parts of the data it receives (see below) |
| `destination` | with `cluster_cases` | Where it sends data, in words, shown before every send |
| `secrets` | no | Names of the server’s environment variables it may read |
| `requires` | no | `packages` (R packages) and `commands` (programs on the `PATH`) it needs |
| `timeout` | no | Seconds the whole plugin may run, 1 to 600; 60 by default |
| `source` | no | An `https://` address of the plugin itself, from which the Plugins screen updates it |

Requirements are checked, never installed: a plugin whose package or
program is missing is listed as unavailable, with the reason, until the
server has what it needs.

### Steps

Each step has a `shell` (`r`, `bash` or `python`) and the code it
`run`s, and may have a `name` and plain settings in `env`:

``` yaml
steps:
  - name: Prepare
    shell: r
    run: |
      writeLines("prepared", "state.txt")
  - name: Upload
    shell: bash
    env:
      MODE: production
    run: |
      curl --fail --silent -T state.txt "$UPLOAD_TARGET"
      echo "message=Uploaded" >> "$EPISODIC_RESULT"
```

`r` runs with the same R installation and packages as EpiSODIC; `bash`
and `python` use the programs on the server. Steps share one working
folder, so one step can leave a file for the next. A step that fails
ends the plugin there, and its error is recorded.

### Placeholders

A step’s `env` can take values from the payload with placeholders, which
saves reading the payload file for a value or two:

``` yaml
    env:
      TITLE: "Outbreak ${{ cluster.label }}: ${{ cluster.pathogen }}"
      SENT_BY: ${{ user.username }}
```

A placeholder names a path in the payload (below), and only parts the
plugin receives: `cluster` needs `cluster_summary`, `run` needs
`run_summary`, and `cases`, being a list, cannot be used. A placeholder
whose value is missing in a run stops that run and says which one,
rather than passing an empty value.

Placeholders work in `env` only, never in `run`. The filled value
reaches your code as an environment variable, as data, so a ward or
pathogen name can never change what the code does. A `${{ }}` in `run`
is reported as a mistake.

### What a step receives

Each step runs in a fresh process with:

- **`EPISODIC_PAYLOAD`**: the path of a JSON file with the data the
  plugin may see (below).
- **`EPISODIC_RESULT`**: the path of the file it reports back to
  (below).
- **`EPISODIC_PLUGIN_ID`** and **`EPISODIC_LANGUAGE`**: the plugin’s id,
  and the language of whoever pushed the button, so a message can be
  written in that language.
- The plugin’s `secrets`, once enabled, and the step’s own `env`.
- What a program needs to start and to reach the network: `PATH`,
  `HOME`, the locale, proxy settings and certificate stores.

Nothing else from the server’s environment is passed, and none of
EpiSODIC’s own variables ever is: a secret whose name starts with
`EPISODIC_` is refused.

### The payload

The payload always holds `api`, `plugin` (`id`, `version`), `trigger`,
`instance_id`, `language` and `dashboard_url`, and for a button the
`user` who pushed it (`username`, `role`). The rest depends on the
plugin’s permissions; a part it has no permission for is simply absent:

| Permission | Trigger | Adds |
|----|----|----|
| `cluster_summary` | button | `cluster`: its id and `label` (`O-12`, in the reader’s language), scale, origin, pathogen, level, care line, region, institution, ward, first and last day, counts, expected, excess, ratio, priority score, detector agreement, opening time and the latest classification |
| `cluster_cases` | button | `cases`: the cluster’s cases as stored, pseudonymised, one object per case |
| `run_summary` | run_committed | `run`: its id, date, status, whether it was the first run on the database, its counts, and `new_clusters` with the summary of each cluster it opened |

`cluster_cases` is the one permission that sends case-level data. A
plugin that asks for it must say where the data goes in `destination`,
and EpiSODIC asks for confirmation, naming that destination, every time
its button is pushed.

### Reporting back

A step reports by adding lines to the file in `EPISODIC_RESULT`:

    status=success
    message=O-12 was shared.
    link=https://regional.example/outbreaks/123

`status` is `success` (the default) or `error`, when the plugin wants to
say that what it tried did not work. `message` is one line of text, and
`link` an `http` or `https` address the dashboard offers as a link. A
later line overrides an earlier one, and any other line is reported as a
mistake in the plugin. A plugin that reports nothing and finishes
cleanly has succeeded without a message.

The dashboard shows the message as plain text and the link as a plain
link, never as markup.

### When something goes wrong

A plugin that fails, or runs longer than its `timeout`, is stopped and
recorded, with the end of its error output. Whoever pushed the button
sees that output straight away, and an administrator finds it on the
Plugins screen, always with the values of the plugin’s secrets masked. A
plugin that runs after detection runs can never make a run fail, and one
failing plugin never stops the others.

### Limitations

What a plugin cannot do, by design:

- It never receives EpiSODIC’s own settings or credentials (the
  database, mail, Microsoft 365 or the import and export keys), so it
  cannot read or write the database, send mail through EpiSODIC’s
  channels, or create an export file of its own.
- It works with what its permissions put in the payload: a cluster’s
  summary and cases, or a run’s summary. Anything else EpiSODIC holds is
  out of its reach.
- It cannot change what EpiSODIC detects, how clusters are reconciled or
  how they are assessed: it acts on results, never on detection.
- It cannot add screens or panels to the dashboard; a button plugin adds
  a button and shows a message and a link.
- It cannot install software: what it `requires` must already be on the
  server.
- It runs for at most 600 seconds, and a button plugin keeps the person
  who pushed it waiting until it finishes.
- Every secret it names must be set on the server; there are no optional
  secrets.
- It cannot be trusted further than its own code: EpiSODIC narrows what
  a plugin receives and records what it did, but cannot stop it sending
  what it received somewhere other than its `destination` says.

### Versions

The plugin API version changes only when the file format, the payload or
the result changes incompatibly. A plugin written for another version is
listed as unavailable with the reason, rather than handed data it might
misread. When you publish a plugin, point its `source` at a release tag
or commit rather than a branch, so an update is a deliberate step for
everyone who uses it.

## See also

- [**Importing and exporting data between
  instances**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/import-export.html)
  for sharing case data with another EpiSODIC instance, which needs no
  plugin.
- [**Notifications**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/notifications.html)
  for alerts on new clusters through the built-in channels.
- [**Deployment**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.html)
  for the configuration file and administrator accounts.
- [**Environment
  variables**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.html)
  for `EPISODIC_PLUGIN_DIR` and every other `EPISODIC_*` variable.
- [`?episodic_plugin_install`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_install.md),
  [`?episodic_plugin_enable`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_enable.md)
  and
  [`?episodic_plugin_run`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_plugin_run.md)
  for the details of each function.
