if ! command -v "stack"; then
    wget -qO- https://get.haskellstack.org/ | sh 
fi


if ! command -v "git"; then
    if command -v "port"; then
        sudo port install git
    else
        if command -v "brew"; then
            sudo brew install git
        else
            echo No known package managers installed from this list: brew,port
        fi
    fi
fi

cd CMips
stack install

