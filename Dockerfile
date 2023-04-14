FROM debian:unstable
LABEL maintainer="Dimitri Fontaine <dim@tapoueh.org>"

RUN apt-get update
RUN apt-get install -y --no-install-recommends          \
                    devscripts debhelper debianutils    \
                    cdbs a2ps liburi-perl fakeroot      \
                    lintian build-essential quilt       \
                    dh-lisp gnupg gnupg-agent dput      \
                    libssl-dev pinentry-tty sudo        \
                    cl-asdf cl-flexi-streams cl-ppcre   \
                    wget curl make git bzip2 time       \
                    ca-certificates                     \
                    libzip-dev libssl1.1 openssl        \
                    patch unzip libsqlite3-dev gawk     \
                    freetds-dev sbcl

RUN useradd --user-group --create-home --home /home/dim --shell /bin/bash --groups tty,sudo dim

RUN install -o dim -m 0700 -d /home/dim/.gnupg
ADD conf/prepare.sh     /root/prepare.sh
ADD conf/bashrc.sh      /home/dim/.bashrc
ADD conf/gpg-agent.conf /home/dim/.gnupg/gpg-agent.conf
ADD conf/devscripts     /home/dim/.devscripts
ADD conf/gpg.conf       /home/dim/.gnupg/gpg.conf
ADD conf/dput.cf        /home/dim/.dput.cf
ADD conf/ql-to-deb.conf /home/dim/.config/common-lisp/source-registry.conf.d/ql-to-deb.conf

ADD . /home/dim/ql-to-deb
RUN chown -R dim:dim /home/dim

USER dim
ENV DEBEMAIL "dim@tapoueh.org"
ENV DEBFULLNAME "Dimitri Fontaine"
ENV DEBSIGN_KEYID "60B1CB4E"

WORKDIR /home/dim/ql-to-deb
RUN mkdir -p ./build/bin
RUN make
WORKDIR /home/dim

USER root
RUN cp /home/dim/ql-to-deb/build/bin/ql-to-deb /usr/local/bin

USER dim
RUN ql-to-deb --version
RUN ql-to-deb --verbose check
RUN ql-to-deb --verbose --on-error-stop update
RUN ql-to-deb --verbose lint

USER root
RUN ql-to-deb --verbose --changes /home/dim/.ql-to-deb.changes install
RUN apt install -y --fix-broken
