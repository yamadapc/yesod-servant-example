./lib/YesodServantExample.md: ./lib/YesodServantExample.lhs
	pandoc lib/YesodServantExample.lhs -f markdown+lhs -t markdown_github -o lib/YesodServantExample.md
	sed -i backup "s/\` sourceCode/\`haskell/" ./lib/YesodServantExample.md
	rm ./lib/YesodServantExample.mdbackup
	make gist

gist: FORCE
	gist -u "https://gist.github.com/yamadapc/4fd6eb3daeec3381e92f5b4679a216c2" -p lib/YesodServantExample.md

FORCE:
