
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

#隐藏报错
typeset -g POWERLEVEL10K_INSTANT_PROMPT=quiet
#强制指定终端色彩解决终端色彩不够256色问题
export TERM="xterm-256color"

#字体设定 (注意，字体设定必须放在主题之前）
POWERLEVEL10K_MODE='nerdfont-complete'

#主题设定
ZSH_THEME="powerlevel10k/powerlevel10k"

#插件设定
plugins=(
  git
  autojump
  zsh-autosuggestions
  zsh-syntax-highlighting
  sudo
)

bindkey '^H' backward-kill-word

source $ZSH/oh-my-zsh.sh

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


. /opt/asdf-vm/asdf.sh

export IDEA_HOME=/home/admin/idea
export PATH=:$PATH:${IDEA_HOME}/bin


