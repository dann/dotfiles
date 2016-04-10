dann's dotfiles
========================== 

INSTALLATION
---------------------------------------

### dotfiles
# Install dotfiles

    % git clone https://github.com/dann/dotfiles.git ~/.dotfiles
    % cd ~/.dotfiles
    % ./dotsetup.sh

# Install coreutils and some unix tools to use this dotfiles to use dotfiles

    % cd ~/.dotfiles/setup
    % ./setup_app_for_mac.sh

### Setup perl and perl modules

    % cd ~/.dotfiles/dotfiles/setup
    % ./setup_perl.sh 
    % ./setup/setup_perl_modules.sh 

### Setup minicpan and fetch perl modules to local

    % perl `which minicpan` -r http://cpan.yahoo.com/ -l ~/share/minicpan

CUSTOMIZATION
---------------------------------------

### Customize zsh configuration

Create 
    ~/.zshr-after or ~/.zshrc-before

These are hook files for .zshrc.
You can customize zsh with these configs.

### Customize vim configuration
Create 
    ~/.vimrc-after or ~/.vimrc-before

USAGE
---------------------------------------

### zsh

  glob history search  `<C-r>` ex) C-r v*m  

### vim

  Select a buffer `efb`

  Select files `eff`

  Cache tags for completion with neocomplecache `nct`

  perltidy `,pt`

  quickrun `\r`

  Grep buffer `,gb`

  Show tag `,tl`

  Show man `K`
  
### screen

  Share session with other users. `screen -x username/`
  You can share a your screen session with users in .dev_users file.
    
  Select a region  
    `<C-a> tab j`
    `<C-a> tab k`
    `<C-a> tab h` 
    `<C-a> tab h` 

  Select region
    `<C-a> w j`
    `<C-a> w k`
    `<C-a> w h`
    `<C-a> w l`

  Split window
    Vsplit `<C-a> w v`
    split `<C-a> w s`

### html

  close tag `,/`
  expand tag with zencoding `<c-y>,`

AUTHOR
---------------------------------------
dann (techmemo at gmail dot com)


LICENSE
---------------------------------------
Apache License 
