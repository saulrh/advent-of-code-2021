#!/usr/bin/bash

tmp=$(mktemp)
cleanup() {
    rm -f $tmp
}
trap cleanup INT TERM

R CMD BATCH --no-save --no-restore --quiet main.R $tmp
bat $tmp

# Local Variables:
# sh-shell: bash
# End:
