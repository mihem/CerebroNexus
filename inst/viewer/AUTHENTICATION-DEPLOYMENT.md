# Authentication: offline deployment checklist

Full setup, Shiny Server/systemd commands, rotation and troubleshooting:
[Authentication guide](https://mihem.github.io/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.html).

## Linux quick start (including Apps copied from Windows)

From the exported App directory, run `bash viewer/start-linux.sh`. It prepares authentication file permissions for the current user, then starts Viewer. R and the App dependencies must be installed. The script does not read, replace or print secrets, and the normal authentication checks still run.

For Shiny Server/systemd, initialize once with `sudo bash viewer/start-linux.sh --prepare-only --user shiny`, replacing `shiny` with the actual service account, then start the service normally. This changes ownership only on the authentication directory, database and optional local secret; the service account must already be able to traverse the App's parent directories and read its code and data. It never starts R as root.

Both commands can be repeated. Already-correct files are left untouched, including on read-only storage. Incorrect permissions on read-only storage must be prepared before mounting it read-only. If permissions do not persist (for example on some Windows-mounted filesystems), copy the App to a Linux filesystem with POSIX permission support. Linked authentication paths are rejected. Run setup only on a trusted App while it is not being modified.

Direct `shiny::runApp()` and service startup remain supported; they validate authentication but do not repair permissions. Scripted exports using an external passphrase still require that variable in the actual R process; this helper does not load an external secret.

## Authentication files

- Keep `private-data/auth/credentials.sqlite` paired with its original database
  passphrase. The passphrase is not a user's login password.
- Supply the configured passphrase environment variable to the actual R worker,
  not only your interactive shell. A nonempty process variable takes precedence
  over a generated App's local `viewer-auth.env`.
- If a generated App includes `viewer-auth.env`, preserve it privately. Its
  generated format is `CEREBRO_AUTH_PASSPHRASE=<64 lowercase hexadecimal characters>`
  on one line, without quotes or `export`. Restore the original if damaged.
  Scripted exports using an external key do not generate this file.
- On Linux the local secret requires mode `0600`. Recommended modes are `0600`
  for the database and `0700` for its directory. The actual Shiny/R runtime user
  must own or have appropriate access and traverse every parent directory.
- Verify permissions after copying, especially from Windows to Linux. A Windows
  startup does not test POSIX ownership. For read-only storage, correct source
  permissions before deployment or use the Linux entry point above; direct App startup does not change them automatically.
- Keep secrets out of source control and logs. Test valid/invalid login on the
  deployed host, and use HTTPS for access beyond the local machine.
