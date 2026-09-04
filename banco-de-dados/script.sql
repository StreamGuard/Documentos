CREATE TABLE empresa (
    id INT AUTO_INCREMENT PRIMARY KEY,
    razao_social VARCHAR(255) NOT NULL,
    cnpj CHAR(14) NOT NULL UNIQUE,
    codigo_empresa CHAR(8) NOT NULL UNIQUE
);


CREATE TABLE usuario (
    id INT AUTO_INCREMENT PRIMARY KEY,
    empresa_id INT NOT NULL,
    nome VARCHAR(255),
    cargo VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL,
    senha_hash VARCHAR(255) NOT NULL,

    CONSTRAINT fk_usuario_empresa
        FOREIGN KEY (empresa_id)
        REFERENCES empresa(id),

    CONSTRAINT uk_usuario_empresa_email
        UNIQUE (empresa_id, email)
);


CREATE TABLE usuario_stream_guard (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nome VARCHAR(255) NOT NULL,
    cargo VARCHAR(255) NOT NULL,
    email VARCHAR(255),
    senha_hash VARCHAR(255)
);


CREATE TABLE servidor (
    id INT AUTO_INCREMENT PRIMARY KEY,
    empresa_id INT NOT NULL,
    codigo_agente VARCHAR(64) NOT NULL UNIQUE,
    hostname VARCHAR(255) NOT NULL,
    sistema_operacional VARCHAR(100) NOT NULL,
    versao_so VARCHAR(100) NOT NULL,
    arquitetura VARCHAR(50) NOT NULL,
    ultimo_health_check DATETIME NULL,

    CONSTRAINT fk_servidor_empresa
        FOREIGN KEY (empresa_id)
        REFERENCES empresa(id)
);


CREATE TABLE componente (
    id INT AUTO_INCREMENT PRIMARY KEY,
    servidor_id INT NOT NULL,
    tipo VARCHAR(50) NOT NULL,
    nome VARCHAR(255),

    CONSTRAINT fk_componente_servidor
        FOREIGN KEY (servidor_id)
        REFERENCES servidor(id)
);


CREATE TABLE metrica (
    id INT AUTO_INCREMENT PRIMARY KEY,
    componente_id INT NOT NULL,
    tipo VARCHAR(50) NOT NULL,
    nome VARCHAR(255) NOT NULL,
    unidade VARCHAR(20) NOT NULL,

    CONSTRAINT fk_metrica_componente
        FOREIGN KEY (componente_id)
        REFERENCES componente(id)
);


CREATE TABLE config_metrica (
    id INT AUTO_INCREMENT PRIMARY KEY,
    metrica_id INT NOT NULL,
    habilitado BOOLEAN NOT NULL DEFAULT TRUE,
    intervalo_coleta INT NOT NULL,
    limite_maximo DECIMAL(10,2) NOT NULL,
    limite_minimo DECIMAL(10,2) NOT NULL,

    CONSTRAINT fk_config_metrica
        FOREIGN KEY (metrica_id)
        REFERENCES metrica(id),

    CONSTRAINT uk_config_metrica
        UNIQUE (metrica_id)
);
