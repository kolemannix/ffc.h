set -e
set -x

if [ -z "$1" ]; then
  echo "usage: $0 <n>"
  exit 1
fi

year="$(date +%y)"
month="$(date +%m)"
tag="v${year}.${month}.$1"

echo "releasing version ${tag}"
git tag -a "${tag}" -m "${tag}"
git push origin "${tag}"
