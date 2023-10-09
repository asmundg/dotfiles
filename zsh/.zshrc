source ~/antigen.zsh

antigen use oh-my-zsh

antigen bundle nvm
antigen bundle direnv
antigen bundle atuinsh/atuin@main
antigen bundle zsh-users/zsh-autosuggestions

# Syntax highlighting bundle must come last
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme afowler
antigen apply
