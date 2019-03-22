docker stop ai_serv_inst
git pull
docker run -p 5000:5000 --rm --name ai_serv_inst  ai_serv