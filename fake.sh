#!/usr/bin/env bash

fake_obtain_script=".fake/fake_obtain.sh"
if [ ! -f "$fake_obtain_script" ]; then
    curl --fail -L -s -o $fake_obtain_script https://raw.githubusercontent.com/matthid/FAKE/coreclr/script/obtain_fake.sh
fi

. $fake_obtain_script

exec_fake $*
