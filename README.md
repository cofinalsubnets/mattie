mattie is a language for my cat, whose name is mattie :3

mattie is about turning strings into other strings.

here's a simple [rule 110](https://en.wikipedia.org/wiki/Rule_110) stepper:

```
rule110 <- '111 -> '0
         | '110 -> '1
         | '101 -> '1
         | '100 -> '0
         | '011 -> '1
         | '010 -> '1
         | '001 -> '1
         | '000 -> '0
loop <- ... & main | .* -> ""
main <- (.(..)< -> rule110)*! ((.* -> "
")! -> "") -> loop
```

`run-mattie.ss` runs mattie the program in the 1st argument with the 2nd
argument as input. the program has to have a main rule.

maybe there will be a better interface & docs l8r, idk

