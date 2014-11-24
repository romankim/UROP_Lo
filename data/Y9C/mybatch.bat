@echo

pause

for /l %%y in (2012,1,2013) do (
	for /l %%q in (1,1,4) do (
		curl -ko ./bhcdata%%y_%%q.zip "https://www.chicagofed.org/applications/bhc_data/bhcdata_create_output.cfm?DYR=%%y&DQTR=%%q"
	)
)


pause