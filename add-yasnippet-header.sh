#!/usr/bin/env bash

shopt -s nullglob
for file in ./yasnippet-snippets/snippets/dockerfile-mode/*; do
    ex -sc '1i|# -*- mode: snippet -*' -cx "$file"
done
