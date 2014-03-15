#!/usr/bin/env bash
read    -p "Username: " user
read -s -p "Password: " password

token=$(curl -F "name=$user"                                                  \
             -F "password=$password"                                          \
          http://www.marmalade-repo.org/v1/users/login                        \
      | jq --raw-output '.token')

curl -F "name=$user"                                                          \
     -F "token=$token"                                                        \
     -F "package=@$1"                                                         \
  http://www.marmalade-repo.org/v1/packages
