if ! command -v "stack"; then
    wget -qO- https://get.haskellstack.org/ | sh 
fi


if ! command -v "git"; then
    if command -v "aptitude"; then
        sudo aptitude install git
    else
        if command -v "apt"; then
            sudo apt install git
        else
            if command -v "apt-get"; then
                sudo apt-get install git
            else
                echo No known package managers installed from this list: apt-get,apt,aptitude
            fi
        fi
    fi
fi

cd CMips
stack install

