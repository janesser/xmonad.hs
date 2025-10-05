#!/bin/bash

GIT_FULLNAME=`git config --global user.name`
GIT_EMAIL=`git config --global user.email`

if [ -z "$GIT_FULLNAME" ] || [ -z "$GIT_EMAIL" ]
then
    echo Git user details empty !

    read -p "Full Name: " GIT_FULLNAME
    read -p "Email: " GIT_EMAIL

    git config --global user.name "$GIT_FULLNAME"
    git config --global user.email "$GIT_EMAIL"

    echo Git user details filled $GIT_FULLNAME \<$GIT_EMAIL\>.
fi