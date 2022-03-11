CREATE USER judge_user WITH PASSWORD 'judge_password';
CREATE DATABASE judge_db WITH OWNER = judge_user ENCODING = 'UTF8';

\c judge_db

GRANT ALL ON DATABASE judge_db TO judge_user;

CREATE TABLE "team" (
	"id" serial PRIMARY KEY,
	"name" varchar,
	"repository_full_name" varchar UNIQUE,
	"technology" varchar,
	"quera_email" varchar UNIQUE
);

CREATE TABLE "team_member" (
	"id" serial PRIMARY KEY,
	"name" varchar,
	"city" varchar,
	"university" varchar,
	"phone_number" varchar,
	"email" varchar,
	"is_leader" boolean,
	"team_id" integer NOT NULL,
	CONSTRAINT fk_team_id FOREIGN KEY ("team_id") REFERENCES "team" ("id")
);

CREATE TABLE "submission" (
	"delivery_guid" varchar(36) PRIMARY KEY,
	"repository_full_name" varchar NOT NULL,
	"repository_name" varchar NOT NULL,
	"repository_clone_url" varchar NOT NULL,
	"repository_owner_login" varchar NOT NULL,
	"head_commit_id" varchar NOT NULL,
	"head_commit_message" varchar NOT NULL,
	"head_commit_timestamp" varchar NOT NULL,
	"pushed_at_timestamp" integer NOT NULL,
	"status" varchar NOT NULL,
	"score" integer,
	"team_id" integer,
	CONSTRAINT fk_team_id FOREIGN KEY ("team_id") REFERENCES "team" ("id")
);


