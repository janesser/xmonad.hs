#!/usr/bin/fish

if test -e ~/.ssh/my-env
    # silence for scp, see https://web.archive.org/web/20191021051539/ https://www.complang.tuwien.ac.at/doc/openssh-server/faq.html#2.9  
    source ~/.ssh/my-env >/dev/null
end
