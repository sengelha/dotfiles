# zshrc
#
# Interactive ZSH login script.  Sets up zsh for interactive use, including
# aliases, colors, prompts, key bindings, tab completion, etc.

# Mac OS X fink support
if [[ -r /sw/bin/init.sh ]]; then
  source /sw/bin/init.sh
fi

[[ -r $HOME/.zsh/zshfunc ]] && source $HOME/.zsh/zshfunc
[[ -r $HOME/.zsh/zshvars ]] && source $HOME/.zsh/zshvars
[[ -r $HOME/.zsh/zshopts ]] && source $HOME/.zsh/zshopts
[[ -r $HOME/.zsh/zshalias ]] && source $HOME/.zsh/zshalias
[[ -r $HOME/.zsh/zshcomp ]] && source $HOME/.zsh/zshcomp

unlimit
limit stack 8192
limit -s
umask 077

watch=(notme)

bindkey -e               # emacs key bindings
bindkey ' ' magic-space  # also do history expansion on space

if isexec dircolors; then
  eval `dircolors -b`
fi

ssh_start
run_kinit
run_fortune
