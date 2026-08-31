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

## Examples

``` r
if (FALSE) { # \dontrun{
episodic_setup_microsoft365(tenant_id = "your-tenant-id")
} # }
```
