# Compile Pascal to C
## Build
```
bison -d parser.y  
flex -i pascal.l  
gcc pascal.tab.c lex.yy.c -o pascal -lfl
```
# Run
`./pascal < input`
