# Importing and exporting data between instances

Two laboratories, or a laboratory and a regional partner, can share
their surveillance data so that each runs detection on the combined
cases. One EpiSODIC instance exports the cases of a date range to a
single encrypted, signed file; the other imports it, and its next
detection run takes the cases in alongside its own.

The file is safe to keep in a public place, such as a public bucket, a
shared drive or a Git repository. Without a recipient’s key it reveals
nothing beyond its own size: not who sent it, not who may read it, not
which period it covers. Without the sender’s signing key nobody can
forge it or alter it unnoticed. Everything works headless, from
`Rscript` or a scheduled job; the dashboard adds a download button,
schedules and an audit trail on top, but is never required.

What is exported and imported is case data only: the rows of
`episodic_case`, with each case’s institution. Clusters, assessments and
declarations are never exported. Their identifiers are assigned by each
database, and their streams depend on each instance’s own geography and
lattice, so an imported cluster could not be reconciled against the
receiver’s own. Each instance runs its own detection on the combined
data instead.

## Setting up: identity and keys

Every EpiSODIC database has an **instance identifier**: ten hexadecimal
characters, drawn from a cryptographic random source when the database
is created and never changed afterwards. It is shown on the dashboard’s
Info screen, and read headlessly with:

``` r

episodic_instance_id()
#> [1] "3fa9c01b2e"
```

Each instance then generates its keys, once:

``` r

keys <- episodic_import_export_keygen("/etc/episodic/keys")
```

This writes two key pairs:

| File | What it is | Who has it |
|----|----|----|
| `episodic_import_export_encryption.key` | Secret encryption key: opens files sent to you | You only. Point `EPISODIC_IMPORT_KEY` at it. |
| `episodic_import_export_encryption.pub` | Public encryption key: partners encrypt to it | Every partner who sends you data |
| `episodic_import_export_signing.key` | Secret signing key: signs the files you export | You only. Point `EPISODIC_EXPORT_SIGNING_KEY` at it. |
| `episodic_import_export_signing.pub` | Public signing key: partners verify your files with it | Every partner who receives your data |

The secret key files are written readable by their owner only. Windows
keeps no such permission bits, so there, keep them in a directory only
the account that runs EpiSODIC can read. Keys never belong in the
configuration file: a configuration is exported and stored, and whoever
reads a secret key there can read, or forge, every file.

Give each partner your two `.pub` files and your instance identifier.
Before relying on a partner’s public keys, compare their fingerprints
with the partner over another channel (a telephone call will do):

``` r

episodic_import_export_key_fingerprint("partner_signing.pub")
```

If both sides read the same fingerprint, the file was not swapped on its
way to you.

Generating keys again replaces them. Files encrypted to the old public
key can then no longer be opened, and partners must pin the new signing
key, which is why
[`episodic_import_export_keygen()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_keygen.md)
refuses to overwrite unless told to.

## Exporting

``` r

episodic_cases_export(
  from = "2026-01-01",
  to = "2026-03-31",
  file = "/srv/episodic/to-partner.episodic",
  recipients = c("keys/partner_a_encryption.pub", "keys/partner_b_encryption.pub")
)
```

This writes the cases with a sample date in the range, encrypted to
every recipient listed and signed with the key in
`EPISODIC_EXPORT_SIGNING_KEY`. One file can serve several recipients;
each opens it with their own secret key, and none of them can forge a
file in your name.

There are two ways to encrypt, and the first is strongly preferred:

- **Public keys (`recipients`).** Only the recipients’ secret keys open
  the file. Replacing one partner’s keys involves nobody else.
- **A passphrase (`passphrase`).** The file key is derived from the
  passphrase with scrypt and a random salt per file. Everyone who knows
  the passphrase can read every file encrypted with it, and changing it
  means coordinating every party at once. A passphrase must have at
  least 16 characters, since a file in a public place can be guessed at
  offline for as long as anyone likes.

Both modes sign the file, and an importer refuses it unless your public
signing key is among the senders it has pinned.

### What leaves the instance

- Only **this instance’s own cases**. Cases it imported from a partner
  are never exported: forwarding one partner’s data to another is a
  matter for a data-sharing agreement, not a side effect of exporting a
  date range.
- Cases as the database stores them: **one row per episode**, after the
  sender’s own deduplication. The receiver deduplicates again with its
  own episode settings, but cannot recover positives the sender had
  already folded into an episode.
- The institution as its stored SHA-1 hash, never the raw key.
- The **required case columns always**, and of the optional ones
  (`receipt_date`, `care_line`, `municipality`, `ward`, `specialism`,
  `pc`, `sex`, `age`) only those you choose. A column left out is not in
  the file at all; the file records which columns were left out, and the
  receiver gets them as not recorded (`NA`), never as a default value
  such as `"U"` or `0`.
- Optionally, postcodes cut to their first characters.

The defaults come from the configuration, outside the configuration
hash, because they change what leaves the instance and never what a
detection run computes:

``` yaml
export:
  fields: [receipt_date, care_line, municipality, ward, specialism, pc, sex, age]
  pc_characters: ~
```

`fields` and `pc_characters` override them per export.

Every export is recorded in `episodic_export_event`, failed ones
included: when, from which host and account (or which dashboard user, or
which schedule), the range, the columns, the recipients’ key
fingerprints, the signing key’s fingerprint, the number of cases and the
SHA-256 of the file.

## Importing

``` r

partner <- episodic_cases_import(
  "/srv/episodic/from-partner.episodic",
  trusted_senders = "keys/partner_signing.pub"
)

episodic_run_cron(cases = my_cases, imports = partner)
```

[`episodic_cases_import()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_import.md)
opens the file with the key in `EPISODIC_IMPORT_KEY` (or the
`passphrase`), verifies it in full, and returns its cases as a validated
data frame in the case data format. It does not write to the database.
The case tables belong to the detection run, so imported cases enter the
way all cases do, through
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
with the same checks, deduplication and audit trail, and each import is
recorded in `episodic_import_event` against the run that consumed it.

What the import does to the cases:

- `source_key`, `patient_key` and `lab_number` are **prefixed with the
  sender’s instance identifier and a colon** (`3fa9c01b2e:12345`). Your
  `12345` and a partner’s `12345` are different results, different
  patients and different cultures; without the prefix, the partner’s
  result would be skipped as already present, and two different patients
  would be merged into one.
- `institution_key` is used as the stored hash the file declares it to
  be, never hashed again.
- The cases are stamped with their origin
  (`episodic_case.origin_instance_id`), which is what keeps them out of
  your own exports.

Nothing in a file is returned unless all of it verifies. The import
stops with an error, of class `episodic_import_export_error` with a
`reason` code, on any of these:

| `reason` | What happened |
|----|----|
| `wrong_key` | The file is not encrypted to your key |
| `wrong_passphrase_or_tampered` | The passphrase does not open it. The format cannot tell a wrong passphrase from an altered file. |
| `untrusted_sender` | It is signed by a key you have not pinned |
| `bad_signature` | Its signature does not verify: forged, or altered after signing |
| `tampered` | Its encrypted content fails authentication |
| `truncated` | It was cut short |
| `hash_mismatch` | A table does not match the SHA-256 its manifest records |
| `unsupported_version` | It was written by a newer EpiSODIC; upgrade to read it |
| `not_export_file` | It is not an export file at all |
| `wrong_mode` | A passphrase was given for a key-encrypted file, or the other way round |
| `bad_payload` | It verifies, but its content is not what the format defines |

A run also refuses an import of its own instance’s export (those cases
are already there), an import whose keys no longer carry their sender’s
prefix, and one of your own `source_key` values that collides with an
imported one. Write your own keys without a
`<ten hexadecimal characters>:` prefix, or they cannot be told apart
from a partner’s.

### Geography the receiver cannot place

A partner’s postcodes may fall outside your `EPISODIC_PC_PROVINCE_MAP`
or your areas. Such cases are **kept**: a case that cannot be placed at
a lattice level joins no stream at that level and still counts
everywhere else, exactly as one of your own cases with an unknown
postcode does. The import warns, once per level, how many cases could
not be placed there, so you can decide whether your reference data needs
the partner’s postcodes added. A sender who cut postcodes shorter than
your areas use is warned about too.

## Scheduling: the Import and export screen

An admin account reaches the Import and export screen from its card on
the Instance screen. It shows the instance identifier, offers a download
of an export (built with the dashboard process’s own
`EPISODIC_EXPORT_SIGNING_KEY`), and lists the export schedules, the
import sources and the audit trail of every export and import. Non-admin
accounts see neither the card nor the screen.

**Export schedules** are carried out by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md)
after its detection transaction has committed, the way scheduled reports
are. Each covers a rolling window, the last N days up to the run date,
every M days; the window must be at least as long as the interval, and
is best somewhat longer, so consecutive exports overlap. The overlap is
harmless: imports are append-only on prefixed keys, so a case carried by
two files is taken in once. An export that fails is recorded and logged
and never fails the run.

**Import sources** are fetched by every run before detection, and their
cases enter in the same transaction as your own. A source that fails
(unreachable, a wrong key, a forged file) is recorded with its reason
and marks the run `partial`, with the source and the reason on the run
row; the run goes on with everything else. It is never skipped in
silence.

The dashboard never imports cases itself. A source is a record of where
the next run fetches from; the run takes the cases in.

**Nothing secret is stored.** A schedule or source names the
**environment variable** that holds each key, passphrase, password,
token or secret URL, and the variable is read by the process that uses
it, at the moment it uses it. Set those variables in the environment the
scheduled run starts in; the dashboard’s own environment matters only
for its download button and its connection test.

The **connection test** checks a schedule or source the way the next run
will use it, and writes nothing. For an import source it reads the key
variable, fetches the file and imports it in full, so a passing test
means the next run takes it in. For an export schedule it reads the
signing key and checks the destination without writing to it, since that
would replace the file partners fetch: a directory must exist and be
writable, and an HTTP(S) destination is asked whether it answers and
whether it refuses the credentials outright. Whether an upload is
accepted is known only from the first real export, and the test says so.
The same test runs headless:

``` r

episodic_import_export_test_connection("Partner lab", kind = "source")
```

## Transport

A file path always works. Beyond it, HTTP(S) through the `httr2`
package: `PUT` to upload, `GET` to fetch. That covers WebDAV (Nextcloud,
ownCloud), a pre-signed Amazon S3 URL, an Azure Blob SAS URL and any web
server that accepts a `PUT`. SharePoint, OneDrive and SFTP are not
supported.

Authentication is HTTP basic (a user name variable and a password
variable, as Nextcloud app passwords use) or a bearer token variable. A
URL stored in a schedule or source may carry **no credentials**: no
`user:password@` and no query string, where a pre-signed URL keeps its
signature. A URL that is itself a secret is named by its variable
instead (`env:PARTNER_URL` in the audit trail), and errors never repeat
it.

Name the published file neutrally. The file’s content reveals nothing,
but its name is the first thing anyone sees.

## What importing and exporting do not solve

These are limits of importing and exporting, stated so they are not
discovered later.

**Patients cannot be linked across laboratories.** Each laboratory
pseudonymises its patients its own way, so the same patient tested at
both becomes two patients after an import, and episode deduplication
cannot merge them. Cross-instance duplicates are therefore possible and
inflate counts. Only a jointly agreed pseudonymisation, such as a keyed
hash both laboratories apply with a shared secret before their data
reaches EpiSODIC, would solve this, and that is an organisational
decision rather than something EpiSODIC can do.

**Institutions are matched by key.** The stored hash of an institution’s
key is the same at every instance, so two laboratories that use the same
key for the same hospital, such as a national registry code, see one
hospital. Two that happen to use the same internal code for different
institutions would see one as well. Agree on a shared institution
coding, or keep the codes apart. An import may add an institution the
receiver has never seen, but never renames or retypes one it already
knows.

**Pathogen names are used verbatim.** `pathogen` is free text, so
`"Norovirus"` at one laboratory and `"norovirus GII"` at another become
two separate streams at the receiver. Agree on names with your partners,
or map the partner’s names in your own extract step before the run. For
bacteria and fungi, the AMR package’s
[`AMR::as.mo()`](https://amr-for-r.org/reference/as.mo.html) and
[`AMR::mo_name()`](https://amr-for-r.org/reference/mo_property.html)
normalise names to one taxonomy; AMR does not cover viruses, so viral
pathogen names need an agreed list.

**Imports are append-only.** A case is never updated once it is on file.
If the sender later corrects a case, the receiver keeps the version it
first imported; if the sender retracts one, it remains at the receiver.
Re-exporting a range does not replace what an earlier file delivered.

**History imported after the first run is baseline only.** The first
detection run on a database reports the whole case history it is given
(a backfill). Every later run reports only what falls inside the
detectors’ lookback windows. A partner’s history imported after your
first run therefore serves as baseline for Farrington and MEM, and its
past clusters are not reported. Import a partner’s history in your first
run if you want its past clusters in your archive.

## Data protection

Pseudonymised case-level health data remains personal data under the
GDPR, encrypted or not, and storing an encrypted file in a public place
is still a processing operation. Encryption makes the storage location
safe; it does not replace a data-sharing agreement or a legal basis for
sharing the data, which the parties must have in place before the first
file is sent.

Each export supports data minimisation: leave out the columns the
partner does not need, and cut postcodes to the precision the shared
purpose requires. The file records what was left out, so the receiver
knows what it does not have. The audit trail at both ends, the export
record at the sender and the import record at the receiver, documents
who shared what with whom, when, and with which keys.

Keep the secret key files and every variable named by a schedule or
source with the same care as the case data itself, and revoke a
compromised key by generating new keys and sending partners the new
public keys.

## File format

An export file is a versioned envelope around a plain, documented
payload. The payload is deliberately not an R serialisation:
[`unserialize()`](https://rdrr.io/r/base/serialize.html) on bytes from
another party is a code-execution risk whatever signature checked them,
and an RDS file is unreadable outside R.

**Envelope, format version 1.** All integers are unsigned and
big-endian.

| Bytes | Content |
|----|----|
| 8 | Magic: ASCII `EPSDCASE` |
| 2 | Format version: `1` |
| 1 | Mode: `1` public key, `2` passphrase |
| 2 + 80 per recipient | Public-key mode: the number of recipients, then the 32-byte content key sealed to each recipient’s X25519 key (libsodium sealed box), in random order |
| 32 | Passphrase mode: the scrypt salt (`crypto_pwhash_scryptsalsa208sha256`, libsodium’s interactive limits) |
| 24 | Nonce |
| rest | XSalsa20-Poly1305 encryption, under the content key, of: the sender’s 32-byte Ed25519 public key, a 64-byte signature over the plaintext header, that public key and the payload, then the payload |

Nothing else is in plaintext. The sender, the range, the row counts and
the schema version are all inside the encrypted payload.

**Payload, version 1.** A zlib stream (`memCompress(type = "gzip")`) of
one UTF-8 JSON document:

| Field | Content |
|----|----|
| `payload_format`, `payload_version` | `"episodic-cases-payload"`, `1` |
| `sender_instance_id` | The sender’s instance identifier |
| `episodic_version`, `schema_version` | The sender’s EpiSODIC version and database schema version |
| `from`, `to`, `created_at` | The sample-date range and when the file was written |
| `fields`, `omitted_fields` | The case columns the file carries and those it left out |
| `pc_characters` | The postcode truncation, or `null` |
| `hashed_columns` | `["institution_key"]` |
| `own_cases_only` | `true`: imported cases are never exported |
| `tables.cases` | `rows`, `columns` (name to `"character"` or `"integer"`), `sha256`, and `data`: the table as JSON text of its own, an object of equal-length arrays with `null` for a missing value |

`sha256` is the SHA-256 of `data`’s exact UTF-8 bytes, checked for every
table before any table is read. A newer EpiSODIC reads older files; an
older EpiSODIC refuses a newer envelope or payload by name rather than
misreading it.

## See also

- [**Getting your data
  in**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/data-format.html)
  for the case data format an import returns, and the geographic
  reference data that decides where a partner’s cases can be placed.
- [**Deployment**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/deployment.html)
  for scheduling
  [`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md),
  which fetches the import sources and carries out the export schedules,
  and for admin accounts.
- [**Environment
  variables**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/environment-variables.html)
  for `EPISODIC_IMPORT_KEY`, `EPISODIC_EXPORT_SIGNING_KEY` and every
  other `EPISODIC_*` variable.
- [**Tuning detection for your
  catchment**](https://certe-medical-epidemiology.github.io/EpiSODIC/articles/tuning-detection.html)
  for the `export.fields` and `export.pc_characters` configuration keys.
- [`?episodic_cases_export`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_export.md)
  and
  [`?episodic_cases_import`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_cases_import.md)
  for the API-level documentation.
- [`?episodic_import_export_test_connection`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_import_export_test_connection.md)
  for testing a schedule or source headless.
