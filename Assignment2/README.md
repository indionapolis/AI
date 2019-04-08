### Assignment 2 IAI Innopolis 2019

#### Computers Can Do Art!

Student Nikulin Pavel

Professor Dr. J.A.Brown 

### Use case:

* to start server run:     ```python main.py```     then you can go to http://0.0.0.0:5000 and try it out

* to use only EA run:     ```python ep_core.py [ path to picture ]```    then you will get result.jpg image in current directory

### Docker use case:

* to create docker image run: ```docker build -t ai_test [ path to Dockerfile ] ```
* to start server run: ```docker run --rm -p 5000:5000 -v [ your local folder for results ]:/app/uploads ai_test``` and go to <http://0.0.0.0:5000/>
* to use only EA run: ```docker run --rm -v [ your local folder with picture ]:/app/img -v [ your local folder for results ]:/app/uploads ai_test python ep_core.py img/[ picture name ]```


### In this repository:

```angular2html
├── Dockerfile - docker file configuration
├── README.md
├── ep_core.py - core algorithm for evolutionary algorithm
├── jud - jud results
├── main.py - flask server
├── neoplast - neoplast results
├── requirements.txt
├── run-servece.sh
├── samples - initial samples
├── src
├── static
├── templates
├── uploads
└── van - van results
```