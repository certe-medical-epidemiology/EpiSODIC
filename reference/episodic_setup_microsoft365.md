# Set up Microsoft 365 authentication for notifications

Runs an interactive Azure AD login and caches the refresh token for
subsequent unattended use by
[`episodic_run_cron()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_run_cron.md).
You only need to run this once per server, unless the token expires.
Requires the `Microsoft365R` and `AzureGraph` packages.

## Usage

``` r
episodic_setup_microsoft365(
  tenant_id,
  client_id = NULL,
  scopes = c("Mail.Send", "User.Read", "openid", "offline_access")
)
```

## Arguments

- tenant_id:

  Your Azure AD tenant ID.

- client_id:

  Your Azure AD app registration's client ID. If `NULL`, uses the
  Microsoft365R package default.

- scopes:

  Scopes to request. The default requests `Mail.Send`.

## Value

Invisible `TRUE` on success.

## Details

If your Azure AD app registration has a client secret configured for
application-level (daemon) access, put the `client_secret` in the
instance YAML instead and skip this function entirely: the cron will use
the client-credentials flow directly.

If your staff already authenticate to Microsoft 365 through a shared
login on this server (for example by having called
[`Microsoft365R::get_business_outlook()`](https://rdrr.io/pkg/Microsoft365R/man/client.html)
interactively at some point, or by running this function once), you can
skip `client_id` entirely in the instance YAML:
`episodic_notify_microsoft365()` will reuse whatever cached Azure AD
token it finds for the configured `tenant_id` in
[`AzureAuth::AzureR_dir()`](https://rdrr.io/pkg/AzureAuth/man/AzureR_dir.html),
without any client ID, client secret, or new interactive login.

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_setup_microsoft365(tenant_id = "your-tenant-id")
} # }
```
