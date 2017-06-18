#!/bin/bash
set -e

if [[ $# -ne 1 ]]; then
    echo "Usage $0: <path to R packages> <packages>"
    exit 1
fi

# from: https://stackoverflow.com/a/23002317/219584
function abspath() {
    if [ -d "$1" ]; then
        (cd "$1"; pwd)
    elif [ -f "$1" ]; then
        if [[ $1 == */* ]]; then
            echo "$(cd "${1%/*}"; pwd)/${1##*/}"
        else
            echo "$(pwd)/$1"
        fi
    fi
}

pkgs_dir=$(abspath $1)

[[ -f "$pkgs_dir/PACKAGES" ]] || Rscript -e "tools::write_PACKAGES('$pkgs_dir')"

N=$(ls -1 "$pkgs_dir" | grep "tar.gz$" | wc -l)
curr=0
installed=0

echo "Installing $N packages from $pkgs_dir"

for pkg in $(ls -1 $pkgs_dir | grep "tar.gz$"); do
    name=$(basename "$pkg")
    name=${name%%.*}

    curr=$((curr+1))

    start=`date +%s`

    if Rscript -e "install.packages('$name', contriburl='file:///$pkgs_dir', type='source', dependencies=c('Depends', 'Imports', 'LinkingTo', 'Suggests'), INSTALL_opts=c('--example', '--install-tests', '--with-keep.source', '--no-multiarch'))"; then
        end=`date +%s`
        time=$((end-start))
        installed=$((installed+1))
        echo "[$curr/$installed/$N] Installed $name"
    else
        echo "[$curr/$installed/$N] Failed $name"
    fi
done
