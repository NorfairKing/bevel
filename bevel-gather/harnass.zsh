# Source this in your ~/.zshrc
autoload -U add-zsh-hook

# Executed before the command
_bevel_preexec(){
  echo "before"
  bevel-gather-before
}

# Executed after the command
_bevel_precmd(){
  echo "after"
  bevel-gather-after
}

echo "Bevel Harnass enabled"

add-zsh-hook preexec _bevel_preexec
add-zsh-hook precmd _bevel_precmd
