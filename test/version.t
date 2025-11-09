Ensures refmt --version prints the right version
  $ refmt --version | cut -d '@' -f 1 | cut -d '-' -f 1 | awk '{$1=$1;print}'
  Reason 3.17.1
