
# Load private config
if [ -f $HOME/.config/fish/before.fish ]
    source $HOME/.config/fish/.fish-before.fish
end

for conf in $HOME/.config/fish/fish.d/*.fish; do
    source $conf;
done


