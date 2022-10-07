COPY (
select date, cycle, amount, recipcode, occupation, employer, orgname, contribid from indivs18
where 
	((contribid <> '') is not FALSE) and (
	(employer like '%BB&T%') or
	(employer like '%BANK OF AMERICA%') or
	(employer like '%BANK OF NEW YORK MELLON%') or
	(employer like '%BNY MELLON%') or
	(employer like 'CIT GROUP%') or
	(employer like 'CITI') or
	(employer like 'CITIGROUP%') or
	(employer like 'CITI GROUP%') or
	(employer like 'CITIBANK%') or
	(employer like 'CITI BANK%') or
	(employer like '%COMERICA%') or
	(employer like 'FIFTH THIRD%') or
	(employer like 'FIRST BANK%') or
	(employer like '%GENERAL ELECTRIC CAPITAL%') or
	(employer like 'GOLDMAN SACHS%') or
	(employer like '%HIBERNIA CORP%') or
	(employer like '%JP MORGAN%') or
	(employer like '%JPM%') or
	(employer like '%JEFFERIES%') or
	(employer like '%KEY BANK%') or
	(employer like '%M&T BANK%') or
	(employer like '%MERRILL LYNCH%') or
	(employer like '%MORGAN STANLEY%') or
	(employer like '%NORTHERN TRUST%') or
	(employer like '%PNC BANK%') or
	(employer like '%REGIONS BANK%') or
	(employer like '%SVB FINANCIAL GROUP%') or
	(employer like '%SOUTHTRUST BANK%') or
	(employer like '%STATE STREET%') or
	(employer like '%US BANCO%') or
	(employer like '%WELLS FARGO%') or
	(employer like '%ZIONS BANCO%'))
) to '/Users/saenger/Desktop/Fall 2021.tmp/1499-final-project-repo/data/2018_cycle.csv' csv header;
