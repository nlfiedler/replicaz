# Replicate ZFS datasets

Replicates ZFS datasets, for the purpose of backing up the data. This implies that the backup is located on-site, and hence not a "real" backup. For off-site backup, see the [akashita](https://github.com/nlfiedler/akashita) project, which uploads select portions of ZFS datasets to Google Cloud Storage.

This application creates a snapshot on the source ZFS file system and sends that in the form of a replication stream to the destination file system. If a previous snapshot created by this application exists then this application will create a new snapshot and send an incremental replication stream to the destination. Older snapshots on both the source and destination will be automatically pruned such that the two most recent are retained.

Note that this application uses the `-F` option for `zfs recv` such that the destination file system is rolled back before receiving the snapshot(s). This is necessary since otherwise the receive will fail due to the mismatch in existing snapshots. This occurs because simply listing a directory in the destination will modify the access times, which causes a write to the file system. The alternative is to make the destination read-only, but that is an extra step which can be easily avoided.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

### Testing

To build and test the application:

```
$ rebar3 ct
```

### Releasing

To produce a self-contained [escript](http://www.erlang.org/doc/man/escript.html), use the `release` make target, like so.

```
$ rebar3 escriptize
```

The result (named `replicaz` in the `_build/default/bin` directory) can be copied to a suitable location (e.g. `/usr/local/bin`) and invoked directly.

## Usage

Invoke like so `replicaz --help` to get the command line help. Two arguments are required, the source dataset and the destination dataset.

```
$ /usr/local/bin/replicaz tank backup
```

Typically this script is run via a cron job.

### Remote Replication

The replicaz script can replicate a ZFS dataset to another system, as long as it also has ZFS support, and `zfs` can be found within a non-interactive shell, and the appropriate SSH configuration has been performed. In particular, create a public/private key pair and copy the public key to the remote system. Then invoke `replicaz` with the `--remote` option and a value of the form `username@hostname`, where `username` is the name of a user with permission to alter ZFS datasets, and `hostname` is the name of the remote system.

```
# ssh-keygen -t rsa
# ssh-copy-id -i ~/.ssh/id_rsa username@hostname
# /usr/local/bin/replicaz --remote username@hostname tank backup
```

If the user for the remote system lacks privileges for running ZFS commands, but has sudo access, then add the `--sudo` option, which will prefix all remote commands with `sudo`. The `--sudo` option only affects remote commands. For local sudo access, run `replicaz` using `sudo`.
