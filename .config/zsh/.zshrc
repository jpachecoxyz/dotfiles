#fancy stuf when open terminal:
# ferris 
# [ -z "$TMUX"  ] && { tmux attach || exec tmux new-session && exit;}
# Enable colors and change prompt:
autoload -U colors && colors	# Load colors

autoload -Uz add-zsh-hook
setopt prompt_subst
add-zsh-hook precmd vcs_info

newline=$'\n'
set_prompt() {
    if [[ $UID -eq 0 ]]; then
        # PROMPT='%1 %F{cyan}%/%f%F{blue}${vcs_info_msg_0_}%f%b ${newline}%F{red}❯❯ %f'
        # PROMPT='%F{red}❯❯ %f'
        PROMPT='%F{red} λ %f'
        RPROMPT='%F{magenta}%~%f%F{magenta}${vcs_info_msg_0_}%f'
    else
        # PROMPT='%1 %F{cyan}%/%f%F{blue}${vcs_info_msg_0_}%f%b ${newline}%F{blue}❯❯ %f'
        # PROMPT='%F{cyan}❯❯ %f'
        PROMPT='%F{magenta}%B(λ jpachecoxyz) -> %b%f '
        RPROMPT='%F{magenta}%~%f%F{magenta}${vcs_info_msg_0_}%f'
    fi
}
# Call the function to set the initial prompt
set_prompt
# Define a precmd function to reset the prompt before each command
precmd() {
    set_prompt
}

# PROMPT='%1 %F{magenta}%/%f%F{blue}${vcs_info_msg_0_}%f%b ${newline}%F{blue}❯❯ %f'
# PROMPT='%1 %F{cyan}%n%f%b %F{cyan}❯❯ %f'
# RPROMPT='%F{magenta}[%2~%f%F{magenta}${vcs_info_msg_0_}]%f'

# Enable checking for (un)staged changes, enabling use of %u and %c
zstyle ':vcs_info:*' check-for-changes true
# Set custom strings for an unstaged vcs repo changes (*) and staged changes (+)
zstyle ':vcs_info:*' unstagedstr ' *'
zstyle ':vcs_info:*' stagedstr ' +'
# Set the format of the Git information for vcs_info
zstyle ':vcs_info:git:*' formats       '-[%b%u%c]'
zstyle ':vcs_info:git:*' actionformats '-[%b|%a%u%c]'

# PS1="%B%{$fg[red]%}[%{$fg[blue]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b >>"


# Uncomment the following line if you want to set the title of the terminal window to the current directory
# precmd() { print -Pn "\e]2;%n@%m:%~\a" }


precmd() {
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"
}

setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

source <(fzf --zsh)

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.cache/zsh/history

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

bindkey -s '^o' 'yazi\n'

bindkey -s '^L' 'lazygit\n'

bindkey -s '^t' 'tmount\n'

bindkey -s '^a' 'bc -lq\n'

bindkey -s '^S' 'void-sv.sh\n'

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M vicmd '^[[P' vi-delete-char
bindkey -M vicmd '^e' edit-command-line
bindkey -M visual '^[[P' vi-delete

# Load syntax highlighting; should be last.
# source ~/.config/zsh/autosuggestions.zsh 2>/dev/null
# source ~/.config/zsh/syntax.zsh 2>/dev/null
source ~/.config/zsh/autosuggestions.zsh 
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# export PYENV_ROOT="$HOME/.pyenv"
# [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init - zsh)"

# Developement
eval "$(direnv hook zsh)"
