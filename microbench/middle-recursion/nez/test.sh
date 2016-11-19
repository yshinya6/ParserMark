for file in `ls ../test/[0-9].txt`; do
  nez mybench -Draw=true -g middle.nez ${file}
  # nez mybench -g ${x}.nez ../test/${x}.txt
done
for file in `ls ../test/[1-5][0-9].txt`; do
  nez mybench -Draw=true -g middle.nez ${file}
  # nez mybench -g ${x}.nez ../test/${x}.txt
done
