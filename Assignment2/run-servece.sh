docker stop ai_serv_inst
git pull
docker rmi -f ai_serv
docker build -t ai_serv .
docker run -d -p 5000:5000 --rm --name ai_serv_inst  ai_serv