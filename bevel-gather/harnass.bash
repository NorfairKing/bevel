# This requires bash-prexec
# https://github.com/rcaloras/bash-preexec

# Executed before the command
_bevel_preexec() {
  echo "before"
}

# Executed after the command
_bevel_precmd() {
  echo "after"
}


preexec_functions+=(_bevel_preexec)
precmd_functions+=(_bevel_precmd)
