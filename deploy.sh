SERVER=ec2-52-90-6-62.compute-1.amazonaws.com
ROUTE=https://ec2-52-90-6-62.compute-1.amazonaws.com # Publicly accessible route to your app
EMAIL=dustin.allen.norwood@gmail.com
  ob deploy init \
  ~/haskell/reflex-realworld-example/deploy \
  --ssh-key ~/.ssh/DustinKeyPair3.pem \
  --hostname $SERVER \
  --route $ROUTE \
  --admin-email $EMAIL
