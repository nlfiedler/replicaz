#
# Use ubuntu as a base in order to easily get ZFS support.
# It is trivial to add a recent Erlang/OTP as well.
#
# This container requires a host with the zfs kernel module.
# i.e. docker-machine will not suffice, but an Ubuntu VM will.
#
FROM ubuntu:latest

#
# We need rebar3, which itself requires Erlang/OTP and git,
# and we need SSH for remote replication.
#
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get -q -y install apt-utils erlang git openssh-client zfsutils-linux
ADD https://s3.amazonaws.com/rebar3/rebar3 /bin/rebar3
#ADD https://github.com/erlang/rebar3/releases/download/3.7.4/rebar3 /bin/rebar3
RUN chmod +x /bin/rebar3

WORKDIR /work

COPY config config
RUN test -f config/id_rsa && chmod 600 config/id_rsa; exit 0
COPY rebar.config .
COPY rebar.config.script .
COPY src src
COPY test test
RUN rebar3 release

ENTRYPOINT [ "/bin/bash" ]
