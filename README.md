# Replicate ZFS datasets

Replicates ZFS datasets, for the purpose of backing up the data. This implies that the backup is located on-site, and hence not a "real" backup. For off-site backup, see the [akashita](https://github.com/nlfiedler/akashita) project, which uploads select portions of ZFS datasets to Google Cloud Storage.

This application creates a snapshot on the source ZFS file system and sends that in the form of a replication stream to the destination file system. If a previous snapshot created by this application exists then this application will create a new snapshot and send an incremental replication stream to the destination. Older snapshots on both the source and destination will be automatically pruned such that the two most recent are retained.

Note that this application uses the `-F` option for `zfs recv` such that the destination file system is rolled back before receiving the snapshot(s). This is necessary since otherwise the receive will fail due to the mismatch in existing snapshots. This may occur because simply listing a directory in the destination will modify the access times, which causes a write to the file system. The alternative is to make the destination read-only, but that is an extra step which can be easily avoided.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R19 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

### Testing

To build and test the application:

```
$ rebar3 ct
```

### Using Vagrant

Unfortunately, a Docker container does not have sufficient privileges to create
or destroy ZFS pools, but it can do just about everything else. As such, testing
can be done within a VM, such as [Ubuntu](https://www.ubuntu.com). This project
uses [Vagrant](https://www.vagrantup.com) and [Fabric](http://www.fabfile.org)
1.x to set up a VM.

```shell
host$ vagrant up
host$ fab provision
host$ vagrant ssh
vagrant$ cd /vagrant
vagrant$ rebar3 clean
vagrant$ rebar3 compile
vagrant$ sudo fallocate -l 64M /mnt/tank
vagrant$ sudo zpool create replicaz /mnt/tank
vagrant$ RPZ_TEST_POOL=replicaz rebar3 ct
vagrant$ sudo zpool destroy replicaz
```

## Deployment

### Docker

The application is easily deployed using [Docker](https://www.docker.com), as
there is a provided `Dockerfile` and `docker-compose.yml` file for building and
running the application in Docker.

1. Put a configuration file, named `user_env.config`, in the `config` directory.
    * See `example.config` in the `config` directory.
1. Craft a "production" Compose file that defines a named volume.
1. Invoke `docker-compose build` to build the container.
1. Deploy to the engine as desired.

## Remote Replication

The replicaz script can replicate a ZFS dataset to another system, as long as it also has ZFS support, and `zfs` can be found within a non-interactive shell, and the appropriate SSH configuration has been performed. In particular, create a public/private key pair and copy the public key to the remote system. Then configure replicaz with appropriate values for the `ssh_host` and `ssh_user` options.

```
# ssh-keygen -t rsa
# ssh-copy-id -i ~/.ssh/id_rsa username@hostname
```

If the user for the remote system lacks privileges for running ZFS commands, but has sudo access, then set the `use_sudo` option to any value. The `use_sudo` option only affects remote commands. For local sudo access, set the `local_sudo` option to any value.
