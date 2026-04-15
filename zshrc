# Add extra directories to path
for d in $HOME/go/bin $HOME/.dotnet/tools $HOME/.cargo/bin /Applications/Docker.app/Contents/Resources/bin; do
  if [[ -d $d ]]; then
    export PATH=$d:$PATH
  fi
done

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="philips"
ZSH_THEME_RANDOM_CANDIDATES=(maran geoffgarside tonotdo michelebologna)

# Enable agent forwarding support
zstyle :omz:plugins:ssh-agent agent-forwarding yes

# Which plugins to load
plugins=(bazel brew dirhistory docker dotnet git golang rust ssh-agent zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
export EDITOR='nvim'
# Custom aliases
alias vim="nvim"

if type fortune &> /dev/null; then
  fortune
fi
