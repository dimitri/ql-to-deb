#!/usr/bin/env bash

if [ ! -f /etc/apt/sources.list.old ]
then
    sudo mv /etc/apt/sources.list /etc/apt/sources.list.old
    sudo cp /vagrant/conf/sources.list /etc/apt/sources.list
fi

sudo apt-get update
sudo apt-get dist-upgrade -y
sudo apt-get install -y devscripts debhelper debianutils dh-lisp \
     gnupg gnupg-agent pinentry-curses

# build deps
sudo apt-get install cl-asdf cl-flexi-streams cl-ppcre cdbs a2ps

curl -O http://pgsql.tapoueh.org/sbcl/sbcl_1.2.0-1_amd64.deb
sudo dpkg -i sbcl_1.2.0-1_amd64.deb

cat /vagrant/conf/bashrc.sh >> ~/.bashrc
