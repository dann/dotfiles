#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load zsh plugins 
#-----------------------------------------------
source ~/.zsh/zgen

if ! zgen saved; then
    zgen oh-my-zsh

    zgen oh-my-zsh plugins/ssh-agent
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/tmux

    zgen load b4b4r07/enhancd
    zgen load zsh-users/zaw
    zgen load mollifier/zload
    zgen load peco/peco

    # Completions
    zgen load zsh-users/zsh-completions
    zgen load glidenote/hub-zsh-completion
    zgen load felixr/docker-zsh-completion

    zgen load joel-porquet/zsh-dircolors-solarized

    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting

    zgen save
fi

#-----------------------------------------------
# Load main settings
#-----------------------------------------------
for conf in $HOME/.zsh/*.zsh; do
    source $conf;
done

#-----------------------------------------------
# Load host specific settings
#-----------------------------------------------
h="${HOST%%.*}"
if [[ -f "$HOME/.zsh/host/$h.zsh" ]]; then
    source "$HOME/.zsh/host/$h.zsh"
fi

#-----------------------------------------------
# Load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after

#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Performance Measurement
#if (which zprof > /dev/null 2>&1) ;then
#  zprof
#fi
