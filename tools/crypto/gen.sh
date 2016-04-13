#!/bin/sh
openssl genrsa -out rsa_pri.key 1024
openssl rsa -in rsa_pri.key -pubout -out rsa_pub.key
cat rsa_pub.key | sed '/^-/d' | gawk '{printf "%s", $1}' > temp_pub.key
openssl dgst -sha1 -binary temp_pub.key |  openssl enc -base64 > rsa_pub.sha
cat rsa_pub.key | sed '/^-/d' | gawk '{printf "%s", $1}' |  openssl aes-128-cbc -e -k "xxiajje-=3iss123" -p -nosalt -out rsa_pub_key.aes
