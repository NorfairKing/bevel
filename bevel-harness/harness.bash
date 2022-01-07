# This requires bash-prexec
# https://github.com/rcaloras/bash-preexec

# Executed before the command
_bevel_preexec() {
  # I don't know why, but $1 contains the whole command.
  local command_id=$(bevel-gather-before "$1")
  export BEVEL_HISTORY_ID="$command_id"
}

# Executed after the command
_bevel_precmd() {
  local EXIT="$?"
  if [[ -z "${BEVEL_HISTORY_ID}" ]]
  then
    return
  fi
  bevel-gather-after "${BEVEL_HISTORY_ID}" "${EXIT}"
}


preexec_functions+=(_bevel_preexec)
precmd_functions+=(_bevel_precmd)

_bevel_cd() {
	tput rmkx
  cd $(bevel cd)
  tput smkx
}

bind -x '"\C-p": _bevel_cd'

_bevel_repeat ()
{
	tput rmkx
  command="$(bevel repeat)"
	tput smkx

  READLINE_LINE=${command}
  READLINE_POINT=${#READLINE_LINE}
}

bind -x '"\C-r": _bevel_repeat'

