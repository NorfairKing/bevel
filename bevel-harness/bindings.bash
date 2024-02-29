_bevel_cd () {
	tput rmkx
  local path="$(bevel-select cd)"
  [[ -z $path ]] || cd "$path"
  tput smkx
}

bind -x '"\C-p": _bevel_cd'


_bevel_repeat () {
	tput rmkx
  command="$(bevel-select repeat)"
	tput smkx

  if [[ $command ]]; then
    READLINE_LINE=${command}
    READLINE_POINT=${#READLINE_LINE}
  fi
}

bind -x '"\C-r": _bevel_repeat'


_bevel_repeat_local () {
	tput rmkx
  command="$(bevel-select repeat-local)"
	tput smkx

  if [[ $command ]]; then
    READLINE_LINE=${command}
    READLINE_POINT=${#READLINE_LINE}
  fi
}

bind -x '"\C-h": _bevel_repeat_local'


_bevel_last () {
  cd $(bevel-select last)
}
