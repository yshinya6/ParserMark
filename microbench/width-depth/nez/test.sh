for width in 20 30 40 50; do
  for x in {0..61}; do
    nez mybench -Draw=true -g ${width}-${x}.nez ../test/${width}-${x}.txt
    # nez mybench -g ${x}.nez ../test/${x}.txt
  done
done
