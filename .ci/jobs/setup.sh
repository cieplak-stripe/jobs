#!/bin/sh

pkg install -y curl
curl -sSL https://get.haskellstack.org/ | sh
su - jobs -c make
