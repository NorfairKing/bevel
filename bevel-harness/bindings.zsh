# [Note: stdin zle widgets]
# The stdin is closed in zle widgets (for whatever reason) so we have to read
# directly from the tty instead of from stdin.
# https://stackoverflow.com/questions/57539180/why-is-interactive-command-breaking-when-using-zsh-widget-to-execute-it

function _bevel_cd {
  zle -I # Invalidate current display
  
  # See [Note: stdin zle widgets]
  cd $(bevel-select </dev/tty)

	zle reset-prompt # Re-expand prompt
}

zle -N _bevel_cd_widget _bevel_cd
bindkey '^p' _bevel_cd_widget

function _bevel_repeat {
  zle -I # Invalidate current display

  # See [Note: stdin zle widgets]
  command=$(bevel repeat </dev/tty)

	if [[ -n $command ]] ; then
		LBUFFER=$command
	fi

	zle reset-prompt
}

zle -N _bevel_repeat_widget _bevel_repeat
bindkey '^r' _bevel_repeat_widget

function _bevel_repeat_local {
  zle -I # Invalidate current display

  # See [Note: stdin zle widgets]
  command=$(bevel repeat-local </dev/tty)

	if [[ -n $command ]] ; then
		LBUFFER=$command
	fi

	zle reset-prompt
}

zle -N _bevel_repeat_local_widget _bevel_repeat_local
bindkey '^h' _bevel_repeat_local_widget


function _bevel_last {
  cd $(bevel last)
}
