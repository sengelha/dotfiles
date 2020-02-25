# zshrc
#
# Interactive ZSH login script.  Sets up zsh for interactive use, including
# aliases, colors, prompts, key bindings, tab completion, etc.

# Initialize colors.
autoload -U colors
colors

# Autoload zsh functions and completions
fpath=(~/.zsh/completion ~/.zsh/functions $fpath)
autoload -U ~/.zsh/functions/*(:t)

# Enable auto-execution of functions.
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

# Append git functions needed for prompt.
preexec_functions+='preexec_update_git_vars'
precmd_functions+='precmd_update_git_vars'
chpwd_functions+='chpwd_update_git_vars'

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
