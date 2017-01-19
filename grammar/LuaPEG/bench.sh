for x in {1..9};do
  lua epsilon.lua ../../source/epsilon/${x}k.epsilon
done

for x in {1..10};do
  lua epsilon.lua ../../source/epsilon/${x}0k.epsilon
done

