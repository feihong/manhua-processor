build:
	dune build

start:
	dune exec ./main.exe

jsonl:
	jq --compact-output '.log.entries[] | {url: .request.url, encoding: .response.content.encoding, data: .response.content.text}' session.har > session.jsonl

db:
	dune exec ./db.exe

cbz:
	dune exec ./cbz.exe

clean:
	rm session.har; rm session.jsonl; rm session.db; rm *.cbz

fmt:
	dune fmt
