CREATE DATABASE IF NOT EXISTS recommendation;
USE recommendation;

CREATE TABLE IF NOT EXISTS rating
(
 userId int,
 movieId int,
 rating int
);


CREATE TABLE IF NOT EXISTS accommodation
(
  id varchar(255),
  title varchar(255),
  location varchar(255),
  price int,
  rooms int,
  rating float,
  type varchar(255)
  PRIMARY KEY (ID)
);
