DROP TABLE indivs18;

CREATE TABLE indivs18
(
    cycle char(4),
    fectransid char(19),
    contribid char(12),
    contrib varchar(50),
    recipid char(9),
    orgname varchar(50),
    ultorg varchar(50),
    realcode char(5),
	date date,
	amount int,
	street varchar(40),
    city varchar(30),
    state char(2),
    zip char(5),
    recipcode char(2),
    type char(3),
    cmteid char(9),
    otherid char(9),
    gender char(1),
    microfilm char(18),
    occupation varchar(38),
    employer varchar(38),
    source char(5)
);

COPY indivs18 from '/Users/saenger/Downloads/CampaignFin18/indivs18-utf8.txt' WITH CSV QUOTE '|' DELIMITER ',';