CREATE DATABASE IF NOT EXISTS recommendation;
USE recommendation;

CREATE TABLE IF NOT EXISTS rating
(
 userId int,
 movieId int,
 rating int
);
