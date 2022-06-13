deploy:
	scp donate.clj saiph:bin/

n:
	bb nrepl-server
