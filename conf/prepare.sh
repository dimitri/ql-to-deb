#! /bin/bash

set -x

chmod g+rw `tty`
chown dim:dim /home/dim/.gnupg/*
ls -l /home/dim/.gnupg/secring.gpg

