#!/usr/bin/env bash

p=".fake";f="$p/fake_obtain.sh";if [ ! -f "$f" ]; then mkdir $p; curl --fail -L -s -o $f https://raw.githubusercontent.com/matthid/FAKE/coreclr/script/obtain_fake.sh; fi; . $f

exec_fake $*
