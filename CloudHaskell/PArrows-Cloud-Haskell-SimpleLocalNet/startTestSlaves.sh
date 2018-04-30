killall Test || echo "True"
./Test slave 127.0.0.1 8001 &
./Test slave 127.0.0.1 8002 &
./Test slave 127.0.0.1 8003 &
./Test slave 127.0.0.1 8004 &
