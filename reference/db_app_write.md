# App-side writers

The app owns the judgements and only ever inserts, never updates and
never deletes. This is what makes the whole concurrency question
disappear: two people assessing the same cluster in the same minute
produce two rows, both visible in the timeline. Nothing in this file
contains an `UPDATE` or a `DELETE` statement; that absence is
load-bearing and should be verified by inspection whenever this file
changes.

## Arguments

- con:

  A
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- cluster_id:

  A cluster id.

- user_id:

  An `episode_app_user` id, or `NA` for a system-authored row.

- verdict:

  One of the five classification values, or `NA`.

- rationale:

  Mandatory free-text rationale.

- wpg_notifiable, ggd_informed:

  Logical or `NA`.

- ggd_note:

  Free text, or `NA`.

- snooze_until:

  A date, or `NA`.

- supersedes:

  An earlier `event_id` this event supersedes, or `NA`.

- stream_id:

  A stream id.

- muted_from, muted_until:

  Mute window bounds (dates).

- reason:

  One of the mute reasons in `episode_stream_mute.reason`.

- note:

  Free text, or `NA`.

- state:

  One of the `episode_cluster_state.state` values.

- trigger:

  One of the `episode_cluster_state.trigger` values.

- event_id:

  The assessment event that caused this transition, or `NA`.

- file_path:

  Path to the rendered report file.

- file_sha256:

  SHA-256 hex digest of the rendered file.

- params_json:

  JSON-serialised render parameters.

- case_ids_json:

  JSON-serialised array of included case ids.

- version_no:

  The report's version number.

- username, full_name, email, password_hash:

  New user's fields.

- role:

  One of `"assessor"`, `"admin"`.

- event_type:

  One of `"login"`, `"password_change"`.
