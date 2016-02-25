# Replicate ZFS datasets

Replicates ZFS storage datasets, for the purpose of backing up the data. This implies that the backup is located on-site, and hence not a "real" backup. For off-site backup, see the [akashita](https://github.com/nlfiedler/akashita) project, which uploads select portions of ZFS datasets to Amazon Glacier.

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R17|R18
* [rebar](https://github.com/rebar/rebar)

### Testing

To build and test the application:

```
$ make test
```

### Releasing

To produce a self-contained [escript](http://www.erlang.org/doc/man/escript.html), use the `release` make target, like so.

```
$ make release
```

The result (named `replicaz`) can be copied to a suitable location (e.g. `/usr/local/bin`) and invoked directly.

## Usage

Invoke like so `replicaz --help` to get the command line help.
