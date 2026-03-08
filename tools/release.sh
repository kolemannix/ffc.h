set -e
set -x

if [ "$#" -ne 0 ]; then
  echo "usage: $0"
  exit 1
fi

make ffc.h
git diff --exit-code ffc.h
tag="$(make -s print_version_tag)"

echo "releasing version ${tag}"
git tag -a "${tag}" -m "${tag}"
git push origin "${tag}"
