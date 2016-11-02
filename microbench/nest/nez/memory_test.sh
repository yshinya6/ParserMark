for x in {0..61}; do
  nez memory -Draw=true -g ${x}.nez ../test/${x}.txt
  # nez mybench -g ${x}.nez ../test/${x}.txt
done
