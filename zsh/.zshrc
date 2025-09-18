source ~/antigen.zsh

if [ "$TERM_PROGRAM" != "vscode" ]; then
    antigen use oh-my-zsh
fi

antigen bundle nvm
antigen bundle direnv
antigen bundle atuinsh/atuin@main
antigen bundle zsh-users/zsh-autosuggestions

# Syntax highlighting bundle must come last
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme afowler

antigen apply

alias rm="trash"

export PATH="${PATH}:/Users/asgramme/.azureauth/0.8.6"
