rm build/ -rf

clj -A:build
cp build/js/app.js ../docs/
cp resources/public/index.html ../docs/

rm build/ -rf
