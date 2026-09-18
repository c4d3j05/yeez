# yeez

A terminal UI for S3 object CRUD, written in Haskell on top of
[brick](https://hackage.haskell.org/package/brick) (TUI framework) and
[amazonka](https://hackage.haskell.org/package/amazonka) (native Haskell AWS
SDK — no shelling out to the `aws` CLI).

```
 yeez — s3://my-bucket/reports/
────────────────────────────────────────────────────────────
  2024/                                                <dir>
  archive/                                             <dir>
  q1-summary.pdf                                       1.4M
  q2-summary.pdf                                       2.1M
────────────────────────────────────────────────────────────
 2 folders, 2 objects
 enter open · esc/h up · u upload · d download · n new folder · R rename · x delete · r refresh · q quit
```

Built and verified with GHC 9.4.7, `cabal` 3.8, `amazonka`/`amazonka-s3` 2.0,
`brick` 2.13 and `vty` 6.6. Every S3 call and every screen transition was
exercised end-to-end against a local S3-compatible server (bucket listing,
folder browsing, upload, download, rename, folder creation and delete).

## Requirements

- GHC and `cabal` (no other system dependencies — no GTK, no browser, no
  `aws` CLI)
- AWS credentials discoverable by `Amazonka.discover`: environment
  variables (`AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` / `AWS_REGION`),
  `~/.aws/credentials`, a container role, or an EC2 instance role

## Build and run

```sh
cabal build
cabal run yeez
```

## Features

The four CRUD operations against S3 objects:

| CRUD op | Feature                   | Keybinding | S3 calls used                          |
|---------|---------------------------|------------|----------------------------------------|
| Create  | Upload a local file       | `u`        | `PutObject`                            |
| Create  | New folder                | `n`        | `PutObject` (zero-byte key ending `/`) |
| Read    | Browse buckets            | (default)  | `ListBuckets`                          |
| Read    | Browse objects/folders    | `Enter`    | `ListObjectsV2` (delimiter `/`)        |
| Read    | Download a file           | `d`        | `GetObject`                            |
| Update  | Rename a file             | `R`        | `CopyObject` + `DeleteObject`          |
| Delete  | Delete a file (confirmed) | `x` → `y`  | `DeleteObject`                         |

S3 has no native "rename" or "folder" concept — rename is implemented as
copy-to-new-key-then-delete-old-key, and folders are synthesized from common
prefixes (`ListObjectsV2` with `delimiter=/`) plus zero-byte marker objects
with a trailing `/`, matching the convention the AWS console and CLI already
use.

## Keybindings

| Key         | Screen             | Action                                              |
|-------------|--------------------|-----------------------------------------------------|
| `↑` / `↓`   | list screens       | move selection                                      |
| `Enter`     | bucket/object list | open selected bucket/folder                         |
| `g`         | bucket list        | open a bucket by name (for bucket-scoped access)    |
| `Esc` / `h` | object list        | go up one level (or back to bucket list)            |
| `u`         | object list        | prompt for a local path, upload into current folder |
| `d`         | object list        | prompt for local destination, download selected file |
| `n`         | object list        | prompt for a name, create folder                    |
| `R`         | object list        | prompt for a new name, rename selected file         |
| `x`         | object list        | ask to confirm, then delete selected file           |
| `r`         | list screens       | refresh current listing                             |
| `c`         | list screens       | open the connection switcher                        |
| `q`         | list screens       | quit                                                |
| `Esc`       | bucket list        | quit                                                |
| `Enter`     | prompt             | submit                                              |
| `Esc`       | prompt/confirm     | cancel, back to object list                         |
| any key     | message            | dismiss, back to the previous screen                |

Paths typed into the prompt may start with `~/`. A download destination that
is an existing directory receives the object under its own name.

## Connections

yeez can hold **several connections open at once** and switch between them
live, without restarting — useful for moving between AWS accounts, regions,
or S3-compatible endpoints (MinIO, R2, Spaces).

Press `c` on the bucket or object list to open the connection switcher:

- `↑`/`↓` and `Enter` — make the highlighted connection active; yeez reloads
  its bucket list immediately
- `a` (or the `+ Add connection…` row) — run the setup wizard on top of the
  running app to add another connection (detected credentials, a named
  profile, or hand-entered keys/region/endpoint), then switch to it
- `Esc` / `c` — close the switcher

The active connection is marked with `●`. At startup the first connection is
whatever the setup wizard (or the ambient credential chain) established.

## Bucket-scoped credentials

Some IAM policies grant access to specific buckets but withhold the
account-wide `s3:ListAllMyBuckets` action. With such credentials the opening
bucket list can't be built, and older behaviour was to treat that as a
connection failure:

```
connection failed: ... not authorized to perform: s3:ListAllMyBuckets ...
```

yeez now recognises this: a `403` / `AccessDenied` on the bucket-list probe
means the credentials are valid but simply can't enumerate buckets, so the
connection still opens. The bucket list is empty, and the status bar prompts
you to press `g` to **open a bucket by name**. Type `my-bucket` (or
`s3://my-bucket/some/prefix`) and yeez browses it directly via
`ListObjectsV2`. If that bucket is also denied, the error is shown and you can
press `g` to try another.

To grant full listing instead, attach a policy allowing `s3:ListAllMyBuckets`
on `*`.

## Custom CA bundles (private or corporate certificates)

Internal S3-compatible endpoints often present a certificate signed by a
private CA — a corporate root, or a TLS-inspecting proxy. The symptom is a
connection that dies on trust grounds:

```
connection failed: ... HandshakeFailed ... certificate has unknown CA
```

yeez treats that as recoverable rather than fatal. Whenever a connection
fails on certificate grounds — at first launch, when picking a profile, or
when adding a connection from the switcher with `c` → `a` — the wizard asks
for a trust store instead of giving up:

```
 Certificate problem
 ... certificate rejected: unknown CA

 Path to a CA bundle in PEM format to trust for this connection:
 ~/certs/corp-root.pem
 enter retry · esc cancel
```

Give it a PEM file (or a directory of them) holding the CA — or the whole
chain — and yeez retries the *same* connection against a trust store built
from that file alone. `esc` backs out and shows the original error.

**The bundle replaces the system trust store for that connection; it does not
add to it.** Certificate and hostname validation still run in full: only the
set of trusted roots changes. A certificate from an unrelated CA, or one
whose name does not match the host being dialled, is still rejected. There is
no "skip verification" option, by design.

When you save the connection as a profile, the path is recorded as
`ca_bundle` so it reconnects without prompting:

```ini
[minio-internal]
aws_access_key_id = ...
aws_secret_access_key = ...
region = us-east-1
endpoint_url = https://minio.corp.example.com:9000
ca_bundle = /Users/me/certs/corp-root.pem
```

`ca_bundle` is the same key the AWS CLI v2 uses, so a profile that already
sets it is picked up by yeez with no extra work.

### Why a file instead of the system store

On macOS the default reader (`x509-system`) shells out to
`security find-certificate -pa` over exactly two keychains —
`SystemRootCertificates.keychain` and `/Library/Keychains/System.keychain`.
It never reads your **login** keychain, which is where a CA lands if you
install it by double-clicking a `.cer`. So a root that every other tool on
the machine trusts can still be invisible to yeez. Pointing at a PEM file
sidesteps the platform reader entirely and behaves identically on macOS,
Linux and in the Docker image.

Existing profiles are never rewritten behind your back: supplying a bundle
for a profile that lacks one fixes the current session, but yeez will ask
again next launch unless you add `ca_bundle` to that profile yourself.

## Screens

The app is a single-window state machine with six screens:

- **Bucket list** (`ScreenBuckets`) — top level, lists all buckets
- **Object list** (`ScreenObjects`) — objects/folders under the current
  bucket + prefix
- **Prompt** (`ScreenPrompt`) — single-line text input, reused for upload
  path / download path / new folder name / rename target
- **Confirm delete** (`ScreenConfirmDelete`) — y/n gate before any delete
- **Message** (`ScreenMessage`) — transient status message, returns to the
  screen it came from on any key
- **Connections** (`ScreenConnections`) — the connection switcher: pick an
  open connection or add a new one, live

## Module layout

```
yeez.cabal
app/Main.hs        -- entry point, just calls UI.App.runApp
src/S3/Client.hs   -- amazonka wrapper: the actual CRUD calls
src/UI/Types.hs    -- shared state/types for the TUI
src/UI/App.hs      -- Brick drawing + event handling, the state machine
```

`S3.Client` exposes the environment constructors (`newAwsEnv`,
`newAwsEnvWith`, `newAwsEnvFromParams`), the connection probe
(`checkConnection`) and the CRUD calls themselves (`listAllBuckets`,
`listObjectsUnder`, `uploadFile`, `downloadFile`, `deleteKey`, `copyKey`,
`createFolderMarker`), plus the TLS helpers `caBundleManager` and
`isCertificateError`. Nothing outside this module calls amazonka — or the
TLS stack — directly, and everything it exposes speaks in `Text` and
`FilePath` rather than in amazonka types.

## Non-goals

- Bucket-level operations (create/delete/configure buckets) — only
  object-level CRUD inside existing buckets
- Multipart upload / resumable transfers for very large files
- Concurrent/background transfers — calls block the UI while running
- Access-control (ACL/policy) management

## Known limitations

- Amazonka's generated field/lens names shift between package versions;
  `src/S3/Client.hs` targets the 2.x line (built against `amazonka-2.0` /
  `amazonka-s3-2.0`) and may need small accessor renames on `cabal build`
  depending on the exact version resolved — every such name is confined to
  that one file
- Blocking IO in the event handler means the UI freezes for the duration of
  any upload/download/list call — fine for small files/listings, not ideal
  for large ones
- Deleting a folder is not offered: `x` only deletes the selected file
