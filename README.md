dann's dotfiles
========================== 

INSTALLATION
---------------------------------------

### dotfiles
    % cd ~/env
    % git clone https://dann@github.com/dann/dotfiles.git
    % cd dotfiles 
    % ./dotsetup.sh

You need to setup coreutils and some unix tools to use this dotfiles.
    % cd ~/env/dotfiles/setup
    % ./setup_app_for_mac.sh

### Setup perl and perl modules

    % cd ~/env/dotfiles/setup
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

  glob history search   ex) C-r v*m
    <C-r> 

### vim

  select buffer, file and recent files with Unite
      <C-f>

  Select a buffer 
      efb

  Select files
      eff

  Cache tags for completion with neocomplecache
      nct

  perltidy
      ,pt
  
### screen

Share session with other users
You can share a your screen session with users in .dev_users file.

    screen -x username/

Create window

  C-a c

Move to the window

  C-a <num>

AUTHOR
---------------------------------------
dann (techmemo at gmail dot com)


LICENSE
---------------------------------------
Apache License 