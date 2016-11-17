for x in {0..61}; do
  nez mybench -Draw=true -g 100-${x}.nez ../test/100-${x}.txt
  # nez mybench -g ${x}.nez ../test/${x}.txt
done
