# Frogs

## Intro

The game http://akidsheart.com/math/mathgames/leapfrog.htm

In the beginning there are frogs facing left, frogs facing right and an empty space between them.
Each frog can jump on the empty space if
 * the frog is facing it and is directly next to it
 * the frog is facing it and is 1 frog away from it

The program solves a frog game for a number of frogs entered by the user.

## Demo

```
$ sbt run

[info] Loading global plugins from /....
[info] Loading project definition from /.../frogs/project
[info] Set current project to Frogs (in build file:/.../frogs/)
[info] Running kdkocev.Main 
frogsCount = 4
>>>>_<<<<
>>>><_<<<
>>>_<><<<
>>_><><<<
>><>_><<<
>><><>_<<
>><><><_<
>><><_<><
>><_<><><
>_<><><><
_><><><><
<>_><><><
<><>_><><
<><><>_><
<><><><>_
<><><><_>
<><><_<>>
<><_<><>>
<_<><><>>
<<_><><>>
<<<>_><>>
<<<><>_>>
<<<><_>>>
<<<_<>>>>
<<<<_>>>>
Elapsed Time = 55 milliseconds
[success] Total time: 7 s, completed
```

## Run

Run in [sbt](http://www.scala-sbt.org/) with the command `run` and enter a number