# Replicate ZFS datasets

Replicates ZFS datasets, for the purpose of backing up the data. This implies that the backup is located on-site, and hence not a "real" backup. For off-site backup, see the [akashita](https://github.com/nlfiedler/akashita) project, which uploads select portions of ZFS datasets to Amazon Glacier.

This application creates a snapshot on the source ZFS file system and sends that in the form of a replication stream to the destination file system. If a previous snapshot created by this application exists then this application will create a new snapshot and send an incremental replication stream to the destination. Older snapshots on both the source and destination will be automatically pruned such that the two most recent are retained.

Note that this application uses the `-F` option for `zfs recv` such that the destination file system is rolled back before receiving the snapshot(s). This is necessary since otherwise the receive will fail due to the mismatch in existing snapshots. This occurs because simply listing a directory in the destination will modify the access times, which causes a write to the file system. The alternative is to make the destination read-only, but that is an extra step which can be easily avoided.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R17|R18
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

### Testing

To build and test the application:

```
$ rebar3 test
```

### Releasing

To produce a self-contained [escript](http://www.erlang.org/doc/man/escript.html), use the `release` make target, like so.

```
$ rebar3 build
```

The result (named `replicaz` in the `_build/default/bin` directory) can be copied to a suitable location (e.g. `/usr/local/bin`) and invoked directly.

## Usage

Invoke like so `replicaz --help` to get the command line help. Two arguments are required, the source dataset and the destination dataset.

```
$ /usr/local/bin/replicaz tank backup
```

Typically this script is run via a cron job.
