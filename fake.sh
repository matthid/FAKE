#!/usr/bin/env bash

# p=".fake";f="$p/obtain_fake.sh";if [ ! -f "$f" ]; then mkdir -p $p; curl --fail -L -s -o $f https://raw.githubusercontent.com/matthid/FAKE/coreclr/script/obtain_fake.sh; fi; . $f
VERBOSE=true

. script/obtain_fake.sh

install_fake_packages

exec_fake $*
