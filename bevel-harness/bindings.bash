_bevel_cd () {
	tput rmkx
  cd $(bevel cd)
  tput smkx
}

bind -x '"\C-p": _bevel_cd'


_bevel_repeat () {
	tput rmkx
  command="$(bevel repeat)"
	tput smkx

  READLINE_LINE=${command}
  READLINE_POINT=${#READLINE_LINE}
}

bind -x '"\C-r": _bevel_repeat'

_bevel_last () {
  cd $(bevel last)
}
