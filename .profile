# If running bash, include it
if [[ -n "${BASH_VERSION}" ]]; then
  test -r "${HOME}/.bashrc" && source "${HOME}/.bashrc"
fi
