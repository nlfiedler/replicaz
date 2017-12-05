# Replicate ZFS datasets

Replicates ZFS datasets, for the purpose of backing up the data. This implies that the backup is located on-site, and hence not a "real" backup. For off-site backup, see the [akashita](https://github.com/nlfiedler/akashita) project, which uploads select portions of ZFS datasets to Google Cloud Storage.

This application creates a snapshot on the source ZFS file system and sends that in the form of a replication stream to the destination file system. If a previous snapshot created by this application exists then this application will create a new snapshot and send an incremental replication stream to the destination. Older snapshots on both the source and destination will be automatically pruned such that the two most recent are retained.

Note that this application uses the `-F` option for `zfs recv` such that the destination file system is rolled back before receiving the snapshot(s). This is necessary since otherwise the receive will fail due to the mismatch in existing snapshots. This may occur because simply listing a directory in the destination will modify the access times, which causes a write to the file system. The alternative is to make the destination read-only, but that is an extra step which can be easily avoided.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R18 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

### Testing

To build and test the application:

```
$ rebar3 ct
```

### Deploying

1. Write a configuration file, named `user_env.config`, at the base of the source tree.
    * See `example.config` in the `docs` directory.
1. Build the release: `rebar3 release`
1. Copy the contents of `_build/default/rel` to the desired installation location.
1. Start it up, likely using `sudo`.
1. Occasionally check the log files in `/opt/replicaz/log`.

For example:

```shell
$ cp ~/replicaz.config user_env.config
$ rebar3 release
$ sudo mkdir -p /opt
$ sudo cp -R _build/default/rel/replicaz /opt
$ sudo /opt/replicaz/bin/replicaz -detached
```

### BSD daemon

See the `config/replicaz.rc` file for an example of managing the replicaz application as a daemon via `rc.d` in BSD systems (in particular FreeBSD, and likely NetBSD as well). You will need to build and deploy the application as described above, and then use the `service` command to start it, as illustrated in `replicaz.rc`.

## Remote Replication

The replicaz script can replicate a ZFS dataset to another system, as long as it also has ZFS support, and `zfs` can be found within a non-interactive shell, and the appropriate SSH configuration has been performed. In particular, create a public/private key pair and copy the public key to the remote system. Then configure replicaz with appropriate values for the `ssh_host` and `ssh_user` options.

```
# ssh-keygen -t rsa
# ssh-copy-id -i ~/.ssh/id_rsa username@hostname
```

If the user for the remote system lacks privileges for running ZFS commands, but has sudo access, then set the `use_sudo` option to any value. The `use_sudo` option only affects remote commands. For local sudo access, set the `local_sudo` option to any value.
