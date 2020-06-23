#!/bin/bash

set -e

if [[ "${TRAVIS_OS_NAME}" == "linux" && "${TRAVIS_BRANCH}" == "master" && "${TRAVIS_PULL_REQUEST}" == "false" ]]; then
  echo -e "Host github.com\n\tStrictHostKeyChecking no\nIdentityFile ~/.ssh/deploy.key\n" >> ~/.ssh/config
  openssl aes-256-cbc -pass "pass:$SERVER_KEY" -pbkdf2 -in ./travis/deploy_key.enc -d -a -out deploy.key
  cp deploy.key ~/.ssh/
  chmod 600 ~/.ssh/deploy.key
  git config --global user.email "yyu@mental.poker"
  git config --global user.name "YOSHIMURA Hikaru"
  git fetch origin gh-pages:gh-pages
  git stash -u
  git checkout gh-pages
  git rm index.html ./js/target/scala-2.13/featherweight_go-fastopt.js
  git checkout master index.html
  git stash pop
  git add index.html ./js/target/scala-2.13/featherweight_go-fastopt.js
  git commit -a -m "auto commit on travis $TRAVIS_JOB_NUMBER $TRAVIS_COMMIT"
  git push git@github.com:y-yu/featherweight_go.git gh-pages:gh-pages
fi
