CREATE TABLE empresa (
    id INT AUTO_INCREMENT PRIMARY KEY,
    razao_social VARCHAR(255) NOT NULL,
    criado_em DATETIME NOT NULL,
    atualizado_em DATETIME DEFAULT NULL
);

CREATE TABLE usuario (
    id INT AUTO_INCREMENT PRIMARY KEY,
    empresa_id INT NOT NULL,
    nome VARCHAR(255) NULL,
    cargo VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL,
    senha VARCHAR(255) NOT NULL,
    CONSTRAINT fk_usuario_empresa FOREIGN KEY (empresa_id) REFERENCES empresa(id)
);

CREATE TABLE usuario_stream_guard (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nome VARCHAR(255) NOT NULL,
    cargo VARCHAR(255) NOT NULL,
    email VARCHAR(255) NULL,
    senha VARCHAR(255) NULL
);

CREATE TABLE servidor (
    id INT AUTO_INCREMENT PRIMARY KEY,
    empresa_id INT NOT NULL,
    hostname VARCHAR(255) NOT NULL,
    sistema_operacional VARCHAR(255) NOT NULL,
    status VARCHAR(255) NOT NULL,
    CONSTRAINT fk_servidor_empresa FOREIGN KEY (empresa_id) REFERENCES empresa(id)
);

CREATE TABLE componente (
    id INT AUTO_INCREMENT PRIMARY KEY,
    servidor_id INT NOT NULL,
    tipo VARCHAR(255) NOT NULL,
    nome VARCHAR(255) NULL,
    CONSTRAINT fk_componente_servidor FOREIGN KEY (servidor_id) REFERENCES servidor(id)
);


CREATE TABLE metrica (
    id INT AUTO_INCREMENT PRIMARY KEY,
    componente_id INT NOT NULL,
    nome VARCHAR(255) NOT NULL,
    CONSTRAINT fk_metrica_componente FOREIGN KEY (componente_id) REFERENCES componente(id)
);

CREATE TABLE config_metrica (
    id INT AUTO_INCREMENT PRIMARY KEY,
    metrica_id INT NOT NULL,
    unidade_medida VARCHAR(255) NULL,
    habilitado BOOLEAN NOT NULL,
    intervalo_coleta INT NOT NULL,
    limite_maximo INT NOT NULL,
    limite_minimo INT NOT NULL,
    CONSTRAINT fk_config_metrica FOREIGN KEY (metrica_id) REFERENCES metrica(id)
);