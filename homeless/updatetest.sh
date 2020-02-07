#!/bin/bash

cd /Users/zhiyulin/zlin18.github.io/homeless && Rscript homelessmap_with_api.R && git rm --cached index.html && git add index.html && git commit * --allow-empty-message -m '' && git push
