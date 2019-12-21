# https://blog.inkdrop.info/my-dev-workflow-using-tmux-vim-video-e30e78a9acce

# change default prefix key
set-option -g prefix C-t

# vim like panel switching
bind -r k select-pane -U 
bind -r j select-pane -D 
bind -r h select-pane -L 
bind -r l select-pane -R

nmap ss :split<Return><C-w>w
nmap sv :vsplit<Return><C-w>w" Move window
map sh <C-w>h
map sk <C-w>k
map sj <C-w>j
map sl <C-w>l" Switch tab
nmap <S-Tab> :tabprev<Return>
nmap <Tab> :tabnext<Return>