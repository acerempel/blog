for file in (rg -g '*.html' --files $argv[1])
    typeset-js $file $file
end
