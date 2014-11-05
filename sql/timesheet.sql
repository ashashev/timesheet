CREATE TABLE categories ( id INTEGER PRIMARY KEY ASC, name TEXT , used INTEGER);
INSERT INTO "categories" VALUES(1,'работа',1);
INSERT INTO "categories" VALUES(2,'перерыв',0);

CREATE TABLE timesheet (
id INTEGER PRIMARY KEY ASC,
date TEXT,
time_from INTEGER,
time_to INTEGER,
category INTEGER,
task_code TEXT,
task_description TEXT,
comment TEXT,
FOREIGN KEY (category) REFERENCES categories (id)
);

