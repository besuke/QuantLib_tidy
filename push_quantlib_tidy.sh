#!/usr/bin/env bash
set -euo pipefail

git add README.md R/bonds_basic/
git commit -m "${1:-add QuantLib SWIG R updates}" || true
git pull --rebase origin "$(git branch --show-current)"
git push origin "$(git branch --show-current)"
