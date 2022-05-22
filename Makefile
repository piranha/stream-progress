deploy:
	scp monofetch.clj saiph:var/monofetch/

n:
	bb nrepl-server
