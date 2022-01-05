# Source this in your ~/.zshrc
autoload -U add-zsh-hook

export BEVEL_HISTORY_ID=""

# Executed before the command
_bevel_preexec(){
  # I don't know why, but $1 contains the whole command.
  local command_id=$(bevel-gather-before "$1")
  export BEVEL_HISTORY_ID="$command_id"
}

# Executed after the command
_bevel_precmd(){
  local EXIT="$?"
  if [[ -z "${BEVEL_HISTORY_ID}" ]]
  then
    return
  fi
  bevel-gather-after "${BEVEL_HISTORY_ID}" "${EXIT}"
}

add-zsh-hook preexec _bevel_preexec
add-zsh-hook precmd _bevel_precmd
