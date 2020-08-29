# Aulas de Cobol

Repositório para armazenar exercícios de cobol



## Installation

Necessário estar com Docker e docker-compose instalado

Instalação do Docker  
> https://docs.docker.com/engine/install/

Instalação do docker-compose
> https://docs.docker.com/compose/install/


Origem da imagem:  
> https://hub.docker.com/r/opensourcecobol/opensource-cobol



## Usage

> Construindo a imagem
```shell
docker-compose up --build -d
```

> Acessando o container:
```shell
docker exec -it cobol /bin/bash
```

> Compilar (de dentro do container):
```shell
cobc HELLO.cbl
```

> Executar (de dentro do container):
```shell
cobcrun HELLO
```


## Contributing
Pull requests são bem vindos.